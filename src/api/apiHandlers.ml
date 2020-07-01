open EzAPI.TYPES
open Api_data.ApiData
open Database_reader_lib
open Database_writer_lib
open Timeline_data

module StringMap = StringCompat.StringMap

module Reader = Reader.Reader_generic (Database_interface.Monad_lwt)

let (>>=) = Lwt.(>>=)

let unauthorized () = EzAPIServerUtils.return (Error ("Error 403"))
let not_found s = EzAPIServerUtils.return (Error ("Error 404: " ^ s))
let unknown_error s = EzAPIServerUtils.return (Error ("Error: " ^ s))
let ok e = EzAPIServerUtils.return (Ok e)

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
    | Error _e -> cont false)

let if_ ~error has args cont =
  has args (fun right -> if right then cont () else error ())

let event (req, id) _ () =
  Lwt_io.printl "CALL event" >>= (fun () ->
    Reader.timeline_of_event id >>= (function
    | Some tid ->
      edition_rights (req, tid) (fun has_rights ->
        if has_rights then
          Reader.event id >>= function
          | Some e -> ok e
          | None -> not_found "[event] Event not found"
        else 
          unauthorized ()
        )
    | None -> not_found "[event] Timeline not found"
        )
    )

let events (req,timeline_id) _ () =
  Lwt_io.printl "CALL events" >>= (fun () ->
    confidential_rights (req, timeline_id) (fun has_rights ->
      Reader.events has_rights timeline_id >>= ok)
    )

let title (req,timeline_id) _ () =
  Lwt_io.printl "CALL title" >>= (fun () ->
    Reader.title timeline_id >>= (function
      | None -> ok None
      | Some (id, title) ->
        if title.confidential then
          confidential_rights (req, timeline_id) (fun has_rights ->
              if has_rights then
                ok (Some (id, title))
              else unauthorized ()
            )
        else ok (Some (id, title))
      )
    )

let add_event (req, timeline_id) _ event =
  Lwt_io.printl "CALL add_event" >>= (fun () ->
    if_ ~error:unauthorized edition_rights (req,timeline_id) (fun () ->
      EzAPIServerUtils.return @@ Writer.add_event event timeline_id
    )
  )

let update_event req _ (id, old_event, event, timeline_id) =
  Lwt_io.printl "CALL update_event" >>= (fun () ->
    Reader.timeline_of_event id >>= (function
    | None -> not_found "[update_event] Event associated to no timeline"
    | Some tid ->
      if tid = timeline_id then
        if_
          ~error:unauthorized
          edition_rights
          (req, tid)
          (fun () ->
             Format.printf "Updating event %i with %a@." id Utils.pp_title event;
             (* Check if the old event has been modified *)
             Reader.event id >>= (function
                 | None ->
                   Format.printf "Deleted element while editing@.";
                   ok (Modified None) 
                 | Some should_be_old_event ->
                   Format.printf "Event in the db: %a@. Expected event: %a@."
                     Utils.pp_title should_be_old_event
                     Utils.pp_title old_event
                   ;
                   if old_event = should_be_old_event then begin (* Todo: merge modifications *)
                     let is_title =
                       match old_event.start_date with
                       | None -> true
                       | Some _ -> false (* May be a title, but that is not important *)
                     in
                     if is_title then begin
                       match Writer.update_title id event with
                       | Ok _s -> ok Success
                       | Error s -> unknown_error s 
                     end else begin
                       match Utils.metaevent_to_event event with
                       | None ->
                         unknown_error "Cannot update an event with a missing start date"
                       | Some e ->
                         match Writer.update_event id e with
                         | Ok _s ->   ok Success
                         | Error s -> unknown_error s
                     end
                   end else begin
                     Format.printf "Modified element while editing@.";
                     ok (Modified (Some should_be_old_event))
                   end
               )
          )
      else
        unauthorized ()
      )
    )

let timeline_data (req, tid) _ () =
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
    edition_rights (req, tid) (fun has_rights ->
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

let remove_event (req, timeline_id) _ () =
  Lwt_io.printl "CALL remove_event" >>= (fun () ->
      match StringMap.find_opt "event_id" req.req_params with
      | None | Some [] -> not_found "Event id"
      | Some (id :: _) ->
        try
          let id = int_of_string id in
          Reader.timeline_of_event id >>= (function
            | None -> not_found "[remove_event] Event does not exist"
            | Some tid ->
              if tid = timeline_id then (* Otherwise, it is possible to easily remove 
                                           all public events *)
                if_ ~error:unauthorized edition_rights (req, tid) (fun () ->
                    EzAPIServerUtils.return @@ Writer.remove_event id
                  )
              else
                unauthorized ()
            )
        with
          _ -> unknown_error "Event id not an integer"
    )

let categories (req, id) _ () =
  Lwt_io.printl "CALL categories" >>= fun () ->
  edition_rights (req, id) (fun rights ->
      ((Reader.categories rights id) >>= ok)
    )

let register_user _ _ (email, pwdhash) =
  Lwt_io.printl "CALL register_user" >>= fun () ->
  EzAPIServerUtils.return (Writer.register_user email pwdhash)
  

let login _ _ (email, pwdhash) =
  Lwt_io.printl "CALL login" >>= fun () ->
  Reader.Login.login email pwdhash >>= function
  | None -> unauthorized ()
  | Some s -> ok s

let logout _ _ (email, cookie) =
  Lwt_io.printl "CALL logout" >>= fun () ->
  Reader.Login.logout email cookie >>= ok


let export_database (req, timeline_id) _ () =
  Lwt_io.printl "CALL export_database" >>= fun () ->
  edition_rights (req, timeline_id) (fun rights ->
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

let create_timeline_lwt req auth title name public =
  match Utils.fopt Utils.hd_opt @@ StringMap.find_opt "auth_email" req.req_params with
  | None ->
    Writer.create_public_timeline title name
  | Some email ->
    if not public && auth then
      Writer.create_private_timeline email title name
    else
      Writer.create_public_timeline title name

let create_timeline (req, name) _ (title, public) =
  Lwt_io.printl "CALL create_database" >>= fun () ->
  let name = Utils.trim name in
  is_auth req (fun auth ->
    EzAPIServerUtils.return @@ create_timeline_lwt req auth title name public
  )

let import_timeline (req, name) _ (title, events, public) =
  Lwt_io.printl "CALL import_database" >>= fun () ->
  let name = Utils.trim name in
  is_auth req (fun auth ->
    match create_timeline_lwt req auth title name public with
  | Error e -> EzAPIServerUtils.return (Error e)
  | Ok tmp_tid ->
    let exception Stop of string in
    let handle_event e =
      match Writer.add_event e tmp_tid with
      | Ok _ -> ()
      | Error s ->
        let _ = Writer.remove_timeline tmp_tid in
        raise (Stop s)
    in
    let add_events =
      try
        List.iter handle_event events;
        let _ = Writer.remove_timeline name in 
        Writer.rename_timeline tmp_tid name
      with
      | Stop s -> Error s
    in
    EzAPIServerUtils.return add_events) 

let user_timelines req _ () =
  Lwt_io.printl "CALL user_timelines" >>= fun () ->
  if_ is_auth ~error:unauthorized req (fun () ->
    match Utils.fopt Utils.hd_opt @@ StringMap.find_opt "auth_email" req.req_params with
    | None ->
      unknown_error "[user_timelines] Error: email should be in params"
    | Some email ->
      Reader.user_timelines email >>= fun l -> ok l
    )

let allow_user req _ (email, timeline_id) =
  Lwt_io.printl "CALL allow_user" >>= fun () ->
  if_ ~error:unauthorized has_admin_rights (req, timeline_id) (fun () ->
    if_ ~error:unauthorized has_admin_rights (req, timeline_id) (fun () ->
      EzAPIServerUtils.return @@ Writer.allow_user_to_timeline email timeline_id
    )
  )

let timeline_users (req,tid) _ () =
  Lwt_io.printl "CALL timeline_users" >>= fun () ->
  if_ ~error:unauthorized edition_rights (req,tid) (fun () ->
      Reader.timeline_users tid >>= fun l -> ok l
  )
  

let remove_user req _ () =
  Lwt_io.printl "CALL remove_user" >>= fun () ->
  if_ ~error:unauthorized is_auth req (fun () ->
    match Utils.fopt Utils.hd_opt @@ StringMap.find_opt "auth_email" req.req_params with
    | None ->
      unknown_error "[user_timelines] Error: email should be in params"
    | Some email -> EzAPIServerUtils.return @@ Writer.remove_user email
  )

let remove_timeline (req,tid) _ () =
  Format.eprintf "CALL remove_timeline";
  if_ ~error:unauthorized edition_rights (req,tid) (fun () -> 
    EzAPIServerUtils.return @@ Writer.remove_timeline tid    
  )

let get_view_token (_, tid) _ () =
  Lwt_io.printl "CALL get_view_token" >>= fun () ->
  Reader.get_view_token tid >>= 
  (function
   | Ok str -> Lwt_io.printl ("Token = " ^ str) >>= (fun () -> ok [str])
   | Error e -> unknown_error e)

let view (req, tid) _ () =
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

let is_auth req _ () =
  Lwt_io.printl "CALL is_auth" >>= fun () ->
  is_auth req ok

let has_admin_rights (req, tid) _ () =
  Lwt_io.printl "CALL has_admin_rights" >>= fun () ->
  has_admin_rights (req, tid) ok

let version _ _ () =
  Lwt_io.printl "CALL version" >>= fun () ->
  ok "0.1"

(*
let reinitialize _ events =
  Writer.remove_events ();
  List.iter Writer.add_event events;
  EzAPIServerUtils.return true
*)
