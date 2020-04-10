open Lwt
open EzAPI.TYPES

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

let add_event req event =
  if_is_auth req (fun () ->
      Writer.add_event event;
      EzAPIServerUtils.return (Ok ())
    )

let update_event req (id, event) =
  if_is_auth
    req
    (fun () ->
       Writer.update_event id event |> EzAPIServerUtils.return
    )

let categories _ () = Reader.categories () >>= EzAPIServerUtils.return

let timeline_data req () =
  let start_date =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "start_date" req.req_params in
  let end_date =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "end_date"   req.req_params in
  let group =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "group"      req.req_params in
  let min_ponderation =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "min_level"  req.req_params in
  let max_ponderation =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "max_level"  req.req_params in
  let confidential =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "confidential"  req.req_params in

  let start_date = Utils.fopt Utils.string_to_date start_date in
  let end_date = Utils.fopt Utils.string_to_date end_date in
  let min_ponderation = Utils.fopt int_of_string_opt min_ponderation in
  let max_ponderation = Utils.fopt int_of_string_opt max_ponderation in
  begin
    match confidential with
    | Some "false" -> Monad_lwt.return false
    | _ -> is_auth req
  end >>= fun confidential ->
  Reader.timeline_data
    ?start_date
    ?end_date
    ?group
    ?min_ponderation
    ?max_ponderation
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
  Reader.login email pwdhash >>= EzAPIServerUtils.return

let is_auth req () = is_auth req >>= EzAPIServerUtils.return 

let reinitialize _ events =
  Writer.remove_events ();
  List.iter Writer.add_event events;
  EzAPIServerUtils.return true
