open EzAPI.TYPES
open Api_data.ApiData
open Database_reader_lib
open Database_writer_lib
open Timeline_data
open Data_types

module StringMap = StringCompat.StringMap

module Reader = Reader.Reader_generic (Database_interface.Monad_lwt)

module Emails = Email.Emails

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

let edition_rights (_req, tid) cont = 
  Reader.filter_of_token tid >>= (function
    | Ok {kind = Edit;_} -> cont true
    | _-> cont false)

let if_ ~error has args cont =
  has args (fun right -> if right then cont () else error ())
(*
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
    ) *)

let add_event (req, timeline_id) _ event =
  Lwt_io.printl "CALL add_event" >>= (fun () ->
    if_ ~error:unauthorized edition_rights (req,timeline_id) (fun () ->
      EzAPIServerUtils.return @@ Writer.add_event event timeline_id
    )
  )

let update_event req _ ((id : int), (old_event : title), (event : title), (timeline_id : string)) :
  (date option update_meta_event_res, sub_event_type) result EzAPIServerUtils.answer Lwt.t =
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
                   if old_event = should_be_old_event then begin
                     (* Todo: merge modifications *)
                     let is_title =
                       match old_event.start_date with
                       | None -> true
                       | Some _ -> false (* May be a title, but that is not important *)
                     in
                     if is_title then begin
                       match Writer.update_title timeline_id id event with
                       | Ok _s -> ok Success
                       | Error s -> unknown_error s 
                     end else begin
                       match Utils.metaevent_to_event event with
                       | None ->
                         unknown_error "Cannot update an event with a missing start date"
                       | Some e ->
                         match Writer.update_event timeline_id id e with
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
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "after" req.req_params in
  let end_date =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "before"   req.req_params in
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
  let min_ponderation = Utils.opt Int32.of_int @@ Utils.fopt int_of_string_opt min_ponderation in
  let max_ponderation = Utils.opt Int32.of_int @@ Utils.fopt int_of_string_opt max_ponderation in
  let tags =
    Utils.fopt (fun str -> if str = "" then None else Some (String.split_on_char ',' str)) tags in
  let confidential =
    match confidential with
    | Some "false" -> false
    | _ -> true in
  edition_rights (req, tid) (fun has_rights ->
      begin
        if confidential then
          Reader.timeline_data
            ~with_confidential:has_rights
            ~tid
            ?start_date
            ?end_date
            ?groups
            ?min_ponderation
            ?max_ponderation
            ?tags
            ()

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
            ()
      end >>= function
      | Ok (title, events) -> EzAPIServerUtils.return (Ok (title, events, has_rights))
      | Error e -> EzAPIServerUtils.return (Error e)
    )
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
                    EzAPIServerUtils.return @@ Writer.remove_event timeline_id id
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

let export_database (_req, timeline_id) _ () =
  Lwt_io.printl "CALL export_database" >>= fun () ->
  Reader.timeline_data ~with_confidential:true ~tid:timeline_id () >>= (function
    | Ok (title, events) -> begin
      try
        let events = List.map snd events in
        let title =
          match title with
          | None -> None
          | Some (_, t) -> Some t in
        let json =
          Json_encoding.construct Data_encoding.timeline_encoding Data_types.{title; events} in
        EzAPIServerUtils.return @@ Ok (Data_encoding.write_json json "www/database.json")
      with Failure s -> EzAPIServerUtils.return (Error s)
    end
    | Error e -> EzAPIServerUtils.return (Error e)
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

let send_link ?lang ~readonly_tid ~email ~timeline_name ~admin_tid =
  let lang =
    match lang with
    | Some "en" -> Some Emails.En
    | Some "fr" -> Some Emails.Fr
    | _ -> None in
  let mail = Emails.creation_email ?lang ~readonly_tid ~email ~timeline_name ~admin_tid in
  Email.Sendgrid_xhr.send_base
    ~api_key:(!Config.Sendgrid.key)
    mail

let create_timeline (req, name) _ (title, public) =
  Lwt_io.printl "CALL create_database" >>= fun () ->
  let name' = Utils.trim name in
  is_auth req (fun auth ->
    match create_timeline_lwt req auth title name' public with
      | Error _ as e -> EzAPIServerUtils.return e
      | (Ok (admin_tid, readonly_tid)) as ok ->
        match Utils.fopt Utils.hd_opt @@ StringMap.find_opt "email" req.req_params with
        | None -> 
          Lwt_io.printl "No email provided, returning" >>= fun () ->
          EzAPIServerUtils.return ok
        | Some email ->
          Lwt_io.printl ("Sending to " ^ email) >>= fun () ->
          let lang = Utils.fopt Utils.hd_opt @@ StringMap.find_opt "lang" req.req_params in
          send_link ?lang ~readonly_tid ~email ~timeline_name:name ~admin_tid >>=
          function
          | Error (id, msg) ->
            Lwt_io.printl (Format.asprintf "Error %i: %a" id Utils.pp_str_opt msg) >>= 
            fun () -> EzAPIServerUtils.return ok
          | Ok _ -> EzAPIServerUtils.return ok 
    )

let import_timeline (req, name) _ (title, events, public) =
  Lwt_io.printl "CALL import_database" >>= fun () ->
  let name = Utils.trim name in
  is_auth req (fun auth ->
    match create_timeline_lwt req auth title name public with
  | Error e -> EzAPIServerUtils.return (Error e)
  | Ok (tmp_tid,_) ->
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
        Lwt_io.printl (Format.sprintf "Renaming %s by %s" tmp_tid name) >>=
        fun _ -> Lwt.return @@ Writer.replace_timeline tmp_tid name
      with
      | Stop s ->
        Lwt.return @@ Error s
    in add_events >>= EzAPIServerUtils.return) 

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

(*
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
*)

let decode_token_params req =
  let after =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "after" req.req_params in
  let before =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "before"   req.req_params in
  let groups = StringMap.find_opt "group" req.req_params in
  let min_ponderation =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "min_level"  req.req_params in
  let max_ponderation =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "max_level"  req.req_params in
  let confidential =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "confidential"  req.req_params in
  let tags =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "tags"  req.req_params in
  let readonly =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "readonly" req.req_params in
  let pretty =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "pretty" req.req_params in

  let after = Utils.fopt Utils.string_to_date after in
  let before = Utils.fopt Utils.string_to_date before in
  let min_level = Utils.opt Int32.of_int @@ Utils.fopt int_of_string_opt min_ponderation in
  let max_level = Utils.opt Int32.of_int @@ Utils.fopt int_of_string_opt max_ponderation in
  let tags =
    Utils.fopt (fun str -> if str = "" then None else Some (String.split_on_char ',' str)) tags in
  let categories = groups in
  let confidential =
    match confidential with
    | Some "false" -> false
    | _ -> true in
  let readonly =
    match readonly with
    | Some "false" -> false
    | _ -> true in
  (after, before, min_level, max_level, tags, categories, confidential, readonly, pretty)

let create_token (req, tid) _ () =
  let after, before, min_level, max_level, tags, categories, confidential, readonly, pretty =
    decode_token_params req in
  
  EzAPIServerUtils.return @@
  Writer.create_token
    ?after ?before
    ?categories ?min_level
    ?max_level ~confidential
    ?tags ~readonly ?pretty
    tid

let update_token_pretty (req, token) _ tid =
  let pretty =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "pretty" req.req_params in
  EzAPIServerUtils.return @@ Writer.update_token_pretty ~pretty ~token tid

let update_token_readonly (req, token) _ tid =
  let _,_,_,_,_,_,_,readonly, _ = decode_token_params req in
  EzAPIServerUtils.return @@
  Writer.update_token_readonly ~readonly ~token tid

let update_token (req, token) _ tid =
  Lwt_io.printl "CALL update_token" >>= (fun () ->
  let after, before, min_level, max_level, tags, categories, confidential, readonly, pretty =
    decode_token_params req in
  let res = 
    Writer.update_token
      ?after ?before
      ?categories ?min_level
      ?max_level ~confidential
      ?tags ~readonly ?pretty
      ~token tid in
  Lwt_io.printl "update_token OK" >>= (fun () ->  
      EzAPIServerUtils.return res
    )
)

let remove_token (_req, token) _ tid =
  EzAPIServerUtils.return @@ Writer.remove_token tid token

let get_tokens (_req, tid) _ () =
  Reader.get_tokens tid >>= EzAPIServerUtils.return

let timeline_name (_req, tid) _ () =
  Lwt_io.printl "CALL timeline_name" >>= fun () ->
  Reader.timeline_name tid >>= EzAPIServerUtils.return

let is_auth req _ () =
  Lwt_io.printl "CALL is_auth" >>= fun () ->
  is_auth req ok

let has_admin_rights (req, tid) _ () =
  Lwt_io.printl "CALL has_admin_rights" >>= fun () ->
  has_admin_rights (req, tid) ok

let update_timeline_name req _ tid =
  match Utils.fopt Utils.hd_opt @@ StringMap.find_opt "pretty" req.req_params with
  | None -> EzAPIServerUtils.return (Error "No name given")
  | Some new_name ->
    EzAPIServerUtils.return @@ Writer.update_timeline_name new_name tid

(* Miscelaneous *)
let version _ _ () =
  Lwt_io.printl "CALL version" >>= fun () ->
  ok "0.1"

(*
let reinitialize _ events =
  Writer.remove_events ();
  List.iter Writer.add_event events;
  EzAPIServerUtils.return true
*)
