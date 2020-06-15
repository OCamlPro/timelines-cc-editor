open Lwt
open EzAPI.TYPES
open ApiData

module StringMap = StringCompat.StringMap

module Reader = Reader.Reader_generic (Monad_lwt)

let unauthorized () = EzAPIServerUtils.return (Error "Error 403")

let is_auth req cont =
  let email =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "auth_email" req.req_params in
  let salted_pwd =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "auth_data" req.req_params in
  match email, salted_pwd with
  | Some email, Some pwd ->
    Reader.is_auth email pwd >>= cont
  | _ -> cont false

let has_admin_rights (req, tid) cont =
  match 
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "auth_email" req.req_params
  with
  | None -> cont false
  | Some e -> Reader.has_admin_rights e tid >>= cont

let confidential_rights (req, tid) cont =
  is_auth req (fun auth ->
    has_admin_rights (req, tid) (fun rights ->
    cont (auth && rights)))

let edition_rights (req, tid) cont = 
  Reader.is_public tid >>= (function
    | Ok is_pub -> 
      if is_pub then 
        cont true
      else
        confidential_rights (req, tid) cont
    | Error e -> cont false)

let if_ ~error has args cont =
  has args (fun right -> if right then cont () else error ())

let event (req, id) () =
  Lwt_io.printl "CALL event" >>= (fun () ->
    Reader.timeline_of_event id >>= (function
    | Some tid ->
      confidential_rights (req, tid) (fun has_rights ->
        if has_rights then
          Reader.event id >>= EzAPIServerUtils.return
        else 
          EzAPIServerUtils.return None
        )
    | None -> EzAPIServerUtils.return None
        )
    )

let events (req,timeline_id) () =
  Lwt_io.printl "CALL events" >>= (fun () ->
    confidential_rights (req, timeline_id) (fun has_rights ->
      Reader.events has_rights timeline_id >>= EzAPIServerUtils.return)
    )

let title (req,timeline_id) () =
  Lwt_io.printl "CALL title" >>= (fun () -> 
    (* To respect privacy, error is the same whether the timeline exists or not. *)
    let error () = EzAPIServerUtils.return (Error "Unknown title") in 
    Reader.title timeline_id >>= (function
      | None -> error ()
      | Some (id, title) ->
        if title.confidential then
          confidential_rights (req, timeline_id) (fun has_rights ->
              if has_rights then
                EzAPIServerUtils.return (Ok (id, title))
              else error ()
            )
        else EzAPIServerUtils.return (Ok (id, title))
      )
    )

let add_event (req, timeline_id) event =
  Lwt_io.printl "CALL add_event" >>= (fun () ->
    if_ ~error:unauthorized edition_rights (req,timeline_id) (fun () ->
      EzAPIServerUtils.return @@ Writer.add_event event timeline_id
    )
  )

let update_event req (id, old_event, event) =
  Lwt_io.printl "CALL update_event" >>= (fun () ->
    Reader.timeline_of_event id >>= (function
    | None -> EzAPIServerUtils.return (Failed "Event associated to no timeline")
    | Some tid ->
      if_
        ~error:(fun () -> EzAPIServerUtils.return (Failed "Error 403"))
        edition_rights
        (req, tid)
        (fun () ->
           Format.printf "Updating event %i with %a@." id Utils.pp_title event;
           (* Check if the old event has been modified *)
           Reader.event id >>= (function
             | None ->
               Format.printf "Deleted element while editing@.";
               EzAPIServerUtils.return (Modified None)
             | Some should_be_old_event ->
               Format.printf "Event in the db: %a@. Expected event: %a@."
                 Utils.pp_title should_be_old_event
                 Utils.pp_title old_event
               ;
               if old_event = should_be_old_event then begin
                 let is_title =
                   match old_event.start_date with
                   | None -> true
                   | Some _ -> false (* May be a title, but that is not important *)
                 in
                 if is_title then begin
                   match Writer.update_title id event with
                   | Ok _s ->   EzAPIServerUtils.return Success
                   | Error s -> EzAPIServerUtils.return (Failed s)
                 end else begin
                   match Utils.metaevent_to_event event with
                   | None ->
                     EzAPIServerUtils.return
                       (Failed "Cannot update an event with a missing start date")
                   | Some e ->
                     match Writer.update_event id e with
                     | Ok _s ->   EzAPIServerUtils.return Success
                     | Error s -> EzAPIServerUtils.return (Failed s)
                 end
               end else begin
                 Format.printf "Modified element while editing@.";
                 EzAPIServerUtils.return (Modified (Some should_be_old_event))
               end
             )
        )
      )
    )

let timeline_data (req, tid) () =
  Lwt_io.printl "CALL timeline_data" >>= (fun () ->
  let start_date =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "start_date" req.req_params in
  let end_date =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "end_date"   req.req_params in
  let groups = StringMap.find_opt "group" req.req_params in
  let min_ponderation =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "min_level"  req.req_params in
  let max_ponderation =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "max_level"  req.req_params in
  let confidential =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "confidential"  req.req_params in
  let tags =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "tags"  req.req_params in

  let start_date = Utils.fopt Utils.string_to_date start_date in
  let end_date = Utils.fopt Utils.string_to_date end_date in
  let min_ponderation = Utils.fopt int_of_string_opt min_ponderation in
  let max_ponderation = Utils.fopt int_of_string_opt max_ponderation in
  let tags =
    Utils.fopt (fun str -> if str = "" then None else Some (String.split_on_char ',' str)) tags in
  let confidential =
    match confidential with
    | Some "false" -> false
    | _ -> true in
  if confidential then
    confidential_rights (req, tid) (fun has_rights ->
    Reader.timeline_data
      ~with_confidential:has_rights
      ~tid
      ?start_date
      ?end_date
      ?groups
      ?min_ponderation
      ?max_ponderation
      ?tags
      () >>= EzAPIServerUtils.return
      )
  else
    Reader.timeline_data
      ~with_confidential:false
      ~tid
      ?start_date
      ?end_date
      ?groups
      ?min_ponderation
      ?max_ponderation
      ?tags
      () >>= EzAPIServerUtils.return
    )

let remove_event (req, id) () =
  Lwt_io.printl "CALL remove_event" >>= (fun () ->
  Reader.timeline_of_event id >>= (function
    | None -> EzAPIServerUtils.return (Error "Event does not exist")
    | Some tid ->
      if_ ~error:unauthorized edition_rights (req, tid) (fun () ->
          EzAPIServerUtils.return @@ Writer.remove_event id
        )
      )
    )

let categories (req, id) () =
  Lwt_io.printl "CALL categories" >>= fun () ->
  confidential_rights (req, id) (fun rights ->
      ((Reader.categories rights id) >>= EzAPIServerUtils.return)
    )

let register_user _ (email, pwdhash) =
  Lwt_io.printl "CALL register_user" >>= fun () ->
  EzAPIServerUtils.return (Writer.register_user email pwdhash)
  

let login _ (email, pwdhash) =
  Lwt_io.printl "CALL login" >>= fun () ->
  Reader.Login.login email pwdhash >>= EzAPIServerUtils.return

let logout _ (email, cookie) =
  Lwt_io.printl "CALL logout" >>= fun () ->
  Reader.Login.logout email cookie >>= EzAPIServerUtils.return


let export_database (req, timeline_id) () =
  Lwt_io.printl "CALL export_database" >>= fun () ->
  confidential_rights (req, timeline_id) (fun rights ->
    Reader.title timeline_id >>= (fun title ->
      Reader.events rights timeline_id >>= (fun events ->
        try
          let events = List.map snd events in
          let title =

            match title with
            | None -> None
            | Some (_, t) -> if rights || not t.confidential then Some t else None in
          let json =
            Json_encoding.construct Data_encoding.timeline_encoding Data_types.{title; events} in
          EzAPIServerUtils.return @@ Ok (Data_encoding.write_json json "www/database.json")
        with Failure s -> EzAPIServerUtils.return (Error s)
      )
    )
  )

let create_timeline (req, name) (title, public) =
  Lwt_io.printl "CALL create_database" >>= fun () ->
  let name = Utils.trim name in
  is_auth req (fun auth ->
    match Utils.fopt Utils.hd_opt @@ StringMap.find_opt "auth_email" req.req_params with
    | None ->
      EzAPIServerUtils.return @@ Writer.create_public_timeline title name
    | Some email ->
      if not public && auth then
        EzAPIServerUtils.return @@ Writer.create_private_timeline email title name
      else
        EzAPIServerUtils.return @@ Writer.create_public_timeline title name
  )

let user_timelines req () =
  Lwt_io.printl "CALL user_timelines" >>= fun () ->
  if_ is_auth ~error:unauthorized req (fun () ->
    match Utils.fopt Utils.hd_opt @@ StringMap.find_opt "auth_email" req.req_params with
    | None ->
      EzAPIServerUtils.return (Error "[user_timelines] Error: email should be in params")
    | Some email ->
      Reader.user_timelines email >>= fun l -> EzAPIServerUtils.return (Ok l)
    )

let allow_user req (email, timeline_id) =
  Lwt_io.printl "CALL allow_user" >>= fun () ->
  if_ ~error:unauthorized has_admin_rights (req, timeline_id) (fun () ->
    if_ ~error:unauthorized has_admin_rights (req, timeline_id) (fun () ->
      EzAPIServerUtils.return @@ Writer.allow_user_to_timeline email timeline_id
    )
  )

let timeline_users (req,tid) () =
  Lwt_io.printl "CALL timeline_users" >>= fun () ->
  if_ ~error:unauthorized edition_rights (req,tid) (fun () ->
      Reader.timeline_users tid >>= fun l -> EzAPIServerUtils.return (Ok l)
  )
  

let remove_user req () =
  Lwt_io.printl "CALL remove_user" >>= fun () ->
  if_ ~error:unauthorized is_auth req (fun () ->
    match Utils.fopt Utils.hd_opt @@ StringMap.find_opt "auth_email" req.req_params with
    | None ->
      EzAPIServerUtils.return (Error "[user_timelines] Error: email should be in params")
    | Some email -> EzAPIServerUtils.return @@ Writer.remove_user email
  )

let remove_timeline (req,tid) () =
  Format.eprintf "CALL remove_timeline";
  if_ ~error:unauthorized edition_rights (req,tid) (fun () -> 
    EzAPIServerUtils.return @@ Writer.remove_timeline tid    
  )

let get_view_token (_, tid) () =
  Lwt_io.printl "CALL get_view_token" >>= fun () ->
  Reader.get_view_token tid >>= 
  (function
   | Ok str -> Lwt_io.printl ("Token = " ^ str) >>= (fun () -> EzAPIServerUtils.return (Ok [str]))
   | Error e -> EzAPIServerUtils.return (Error e))

let view (req, tid) () =
  Lwt_io.printl "CALL view" >>= (fun () ->
      let start_date =
        Utils.fopt Utils.hd_opt @@ StringMap.find_opt "start_date" req.req_params in
      let end_date =
        Utils.fopt Utils.hd_opt @@ StringMap.find_opt "end_date"   req.req_params in
      let groups = StringMap.find_opt "group" req.req_params in
      let min_ponderation =
        Utils.fopt Utils.hd_opt @@ StringMap.find_opt "min_level"  req.req_params in
      let max_ponderation =
        Utils.fopt Utils.hd_opt @@ StringMap.find_opt "max_level"  req.req_params in
      let tags =
        Utils.fopt Utils.hd_opt @@ StringMap.find_opt "tags"  req.req_params in

      let start_date = Utils.fopt Utils.string_to_date start_date in
      let end_date = Utils.fopt Utils.string_to_date end_date in
      let min_ponderation = Utils.fopt int_of_string_opt min_ponderation in
      let max_ponderation = Utils.fopt int_of_string_opt max_ponderation in
      let tags =
        Utils.fopt
          (fun str -> if str = "" then None else Some (String.split_on_char ',' str)) tags in
      Reader.view
        ~tid
        ?start_date
        ?end_date
        ?groups
        ?min_ponderation
        ?max_ponderation
        ?tags
      >>= EzAPIServerUtils.return
    )

let is_auth req () =
  Lwt_io.printl "CALL is_auth" >>= fun () ->
  is_auth req EzAPIServerUtils.return 

let has_admin_rights (req, tid) () =
  Lwt_io.printl "CALL has_admin_rights" >>= fun () ->
  has_admin_rights (req, tid) EzAPIServerUtils.return 

let version _ () =
  Lwt_io.printl "CALL version" >>= fun () ->
  EzAPIServerUtils.return "0.1"

(*
let reinitialize _ events =
  Writer.remove_events ();
  List.iter Writer.add_event events;
  EzAPIServerUtils.return true
*)
