open Lwt
open EzAPI.TYPES
open ApiData

module StringMap = StringCompat.StringMap

module Reader = Reader.Reader_generic (Monad_lwt)

let is_auth req =
  let email =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "auth_email" req.req_params in
  let salted_pwd =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "auth_data" req.req_params in
  match email, salted_pwd with
  | Some email, Some pwd -> Reader.is_auth email pwd
  | _ -> Monad_lwt.return false

let has_admin_rights req tid =
  match 
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "auth_email" req.req_params
  with
  | None -> Monad_lwt.return false
  | Some e -> Reader.has_admin_rights e tid

let if_is_auth req cont =
  is_auth req >>= (fun auth ->
      if auth then
        cont ()
      else
        EzAPIServerUtils.return (Error "Error 403")
    )

let if_has_admin req tid cont =
  has_admin_rights req tid >>= (fun admin ->
      if admin then
        cont ()
      else
        EzAPIServerUtils.return (Error "Error 403")
    )

let event (req, id) () =
  is_auth req >>= fun auth ->
      Reader.timeline_of_event id >>= (function
      | None -> EzAPIServerUtils.return None
      | Some timeline_id ->
        has_admin_rights req timeline_id >>= fun rights ->
        Reader.event auth rights id >>= EzAPIServerUtils.return
    )

let events (req,timeline_id) () =
  is_auth req >>= fun auth ->
  has_admin_rights req timeline_id >>= fun rights ->
  Reader.events auth rights timeline_id >>= EzAPIServerUtils.return

let title (_,timeline_id) () = Reader.title timeline_id >>= EzAPIServerUtils.return

let add_event (req, timeline_id) event =
  if_is_auth req (fun () ->
      if_has_admin req timeline_id (fun () ->
      Writer.add_event event timeline_id;
      EzAPIServerUtils.return (Ok ())
    ))
    
let update_event req (id, old_event, event) =
  is_auth req >>= (fun auth ->
      Reader.timeline_of_event id >>= (function
        | None -> EzAPIServerUtils.return (Failed "Event does not exist")
        | Some timeline_id ->
          has_admin_rights req timeline_id >>= (fun admin ->
            if not (auth && admin) then
              EzAPIServerUtils.return (Failed "Error 403")
            else begin
              Format.printf "Updating event %i with %a@." id Utils.pp_title event;
              (* Check if the old event has been modified *)
              Reader.event true true id >>=
              (function
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
                      | Ok () ->   EzAPIServerUtils.return Success
                      | Error s -> EzAPIServerUtils.return (Failed s)
                    end else begin
                      match Utils.metaevent_to_event event with
                      | None -> EzAPIServerUtils.return (Failed "Cannot update an event with a title (start date is missing)")
                      | Some e ->
                        match Writer.update_event id e with
                        | Ok () ->   EzAPIServerUtils.return Success
                        | Error s -> EzAPIServerUtils.return (Failed s)
                    end
                  end else begin
                    Format.printf "Modified element while editing@.";
                    EzAPIServerUtils.return (Modified (Some should_be_old_event))
                  end
              )
            end
            )
        )
    )

let timeline_data (req, tid) () =
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
  begin
    match confidential with
    | Some "false" -> Monad_lwt.return false
    | _ -> is_auth req
  end >>= fun is_auth ->
    has_admin_rights req tid >>= (fun has_admin_rights ->
    Reader.timeline_data
      ~is_auth
      ~has_admin_rights
      ~tid
      ?start_date
      ?end_date
      ?groups
      ?min_ponderation
      ?max_ponderation
      ?tags
      () >>= EzAPIServerUtils.return)

let remove_event (req, id) () =
  if_is_auth req (fun () ->
      Reader.timeline_of_event id >>= (function
        | None -> EzAPIServerUtils.return (Error "Event does not exist")
        | Some timeline_id ->
          if_has_admin req timeline_id (fun () ->
              let () = Writer.remove_event id in
              (EzAPIServerUtils.return (Ok ()))
            )
        )
    )

let categories (req, id) () =
  is_auth req >>= (fun auth ->
      has_admin_rights req id >>= (fun rights ->
          ((Reader.categories auth rights id) >>= EzAPIServerUtils.return)
        )
    )

let register_user _ (email, pwdhash) =
  EzAPIServerUtils.return (Writer.register_user email pwdhash)

let login _ (email, pwdhash) =
  Reader.Login.login email pwdhash >>= EzAPIServerUtils.return

let logout _ (email, cookie) =
  Reader.Login.logout email cookie >>= EzAPIServerUtils.return

let is_auth req () = is_auth req >>= EzAPIServerUtils.return

let has_admin_rights (req, id) () = has_admin_rights req id >>= EzAPIServerUtils.return

let export_database (req, timeline_id) () =
  if_is_auth req (fun () ->
    if_has_admin req timeline_id (fun () ->
      Reader.title timeline_id >>= fun title ->
      Reader.events true true timeline_id >>= fun events ->
      try
        let events = List.map snd events in
        let json =
          Json_encoding.construct Data_encoding.timeline_encoding Data_types.{title; events} in
        EzAPIServerUtils.return @@ Ok (Data_encoding.write_json json "www/database.json")
      with Failure s -> EzAPIServerUtils.return (Error s)
        )
    )

let create_timeline req title =
  if_is_auth req (fun () ->
    match Utils.fopt Utils.hd_opt @@ StringMap.find_opt "auth_email" req.req_params with
    | None ->
      EzAPIServerUtils.return (Error "[create_timeline] Error: email should be in params")
    | Some email ->
      EzAPIServerUtils.return @@ Writer.create_timeline email title
  )

(*
let reinitialize _ events =
  Writer.remove_events ();
  List.iter Writer.add_event events;
  EzAPIServerUtils.return true
*)
