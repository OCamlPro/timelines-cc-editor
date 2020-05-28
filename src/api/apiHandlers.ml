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

let if_is_auth req cont =
  is_auth req >>= (fun auth ->
      if auth then
        cont ()
      else
        EzAPIServerUtils.return (Error "Error 403")
    )

let event (req, id) () =
  is_auth req >>=
  fun auth -> Reader.event auth id >>= EzAPIServerUtils.return

let events req () =
  is_auth req >>= fun auth ->
  Reader.events auth >>= EzAPIServerUtils.return

let title _ () = Reader.title () >>= EzAPIServerUtils.return

let add_event req event =
  if_is_auth req (fun () ->
      Writer.add_event event;
      EzAPIServerUtils.return (Ok ())
    )

let update_event req (id, old_event, event) =
  is_auth req >>=
  (fun auth ->
     if not auth then EzAPIServerUtils.return (Failed "Error 403")
     else begin
       Format.printf "Updating event %i with %a@." id Utils.pp_event event;
       (* Check if the old event has been modified *)
       Reader.event true id >>=
       (function
         | None ->
           Format.printf "Deleted element while editing@.";
           EzAPIServerUtils.return (Modified None)
         | Some should_be_old_event ->
             Format.printf "Event in the db: %a@. Expected event: %a@."
               Utils.pp_event should_be_old_event
               Utils.pp_event old_event
             ;
           if old_event = should_be_old_event then begin
             match Writer.update_event id event with
             | Ok () ->   EzAPIServerUtils.return Success
             | Error s -> EzAPIServerUtils.return (Failed s)
           end else begin
             Format.printf "Modified element while editing@.";
             EzAPIServerUtils.return (Modified (Some should_be_old_event))
           end
       )
     end
  )

let update_title req (old_title, title) =
  is_auth req >>=
  (fun auth ->
     if not auth then EzAPIServerUtils.return (Failed "Error 403")
     else begin
       (* Check if the old event has been modified *)
       Reader.title () >>=
       (function
         | None ->
           Format.printf "Deleted element while editing@.";
           EzAPIServerUtils.return (Modified None)
         | Some should_be_old_title ->
           Format.printf "Title in the db: %a@. Expected title: %a@."
             Utils.pp_title should_be_old_title
             Utils.pp_title old_title
           ;
           if old_title = should_be_old_title then begin
             match Writer.update_title title with
             | Ok () ->   EzAPIServerUtils.return Success
             | Error s -> EzAPIServerUtils.return (Failed s)
           end else begin
             Format.printf "Modified element while editing@.";
             EzAPIServerUtils.return (Modified (Some should_be_old_title))
           end
       )
     end
  )

let categories _ () = Reader.categories () >>= EzAPIServerUtils.return

let timeline_data req () =
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
  end >>= fun confidential ->
  Reader.timeline_data
    ?start_date
    ?end_date
    ?groups
    ?min_ponderation
    ?max_ponderation
    ?tags
    confidential
    () >>= EzAPIServerUtils.return

let remove_event (req, id) () =
  if_is_auth req
    (fun () ->
       let () = Writer.remove_event id in
       (EzAPIServerUtils.return (Ok ()))
    )

let register_user _ (email, pwdhash) =
  EzAPIServerUtils.return (Writer.register_user email pwdhash)

let login _ (email, pwdhash) =
  Reader.Login.login email pwdhash >>= EzAPIServerUtils.return

let logout _ (email, cookie) =
  Reader.Login.logout email cookie >>= EzAPIServerUtils.return

let is_auth req () = is_auth req >>= EzAPIServerUtils.return

let export_database req () =
  if_is_auth req (fun () ->
      Reader.title () >>= fun title ->
      Reader.events true >>= fun events ->
      try
          let events = List.map snd events in
          let json =
            Json_encoding.construct Data_encoding.timeline_encoding Data_types.{title; events} in
          EzAPIServerUtils.return @@ Ok (Data_encoding.write_json json "www/database.json")
        with Failure s -> EzAPIServerUtils.return (Error s)
    )
let reinitialize _ events =
  Writer.remove_events ();
  List.iter Writer.add_event events;
  EzAPIServerUtils.return true
