(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

open Api_data
open Database_reader_lib
open Database_writer_lib
open Data_types
open Lwt.Infix

module Emails = Email.Emails
module StringMap = EzAPIServerUtils.StringMap
module Reader = Reader.Reader_generic (Database_interface.Monad_lwt)
module Misc = Utils.Misc

type 'a ans =  ('a, string) result EzAPIServerUtils.Answer.t Lwt.t

type ('input, 'res) handler0 =
  EzAPI.Req.t -> EzAPI.Security.basic list -> 'input -> 'res ans

type ('a, 'input, 'res) handler1 =
  (EzAPI.Req.t * 'a) ->
  EzAPI.Security.basic list ->
  'input ->
  'res ans

let unauthorized () = EzAPIServerUtils.return (Error ("Error 403"))
let not_found s = EzAPIServerUtils.return (Error ("Error 404: " ^ s))
let unknown_error s = EzAPIServerUtils.return (Error ("Error: " ^ s))
let ok e = EzAPIServerUtils.return (Ok e)

let get_unique_from_req key req =
  match StringMap.find_opt key req.EzAPI.Req.req_params with
  | None | Some [] -> None
  | Some (k :: _) -> Some k

let get_auth_email = get_unique_from_req "auth_email"

let get_auth_pwd = get_unique_from_req "auth_data"

let is_auth req cont =
  let email = get_auth_email req in
  let salted_pwd = get_auth_pwd req in
  match email, salted_pwd with
  | Some email, Some pwd ->
    Reader.is_auth email pwd >>= cont
  | _ -> cont false

let has_admin_rights (req, tid) cont =
  match get_auth_email req with
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

let add_event (req, timeline_id) _ event =
  Lwt_io.printl "CALL add_event" >>= (fun () ->
    if_ ~error:unauthorized edition_rights (req,timeline_id) (fun () ->
      EzAPIServerUtils.return @@ Writer.add_event event timeline_id
    )
  )

let update_event
    req _ ((id : int), (old_event : title), (event : title), (timeline_id : string)) :
  date option update_meta_event_res ans =
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
             Format.printf "Updating event %i with %a@." id Misc.pp_title event;
             (* Check if the old event has been modified *)
             Reader.event id >>= (function
                 | None ->
                   Format.printf "Deleted element while editing@.";
                   ok (Modified None)
                 | Some should_be_old_event ->
                   Format.printf "Event in the db: %a@. Expected event: %a@."
                     Misc.pp_title should_be_old_event
                     Misc.pp_title old_event
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
                       match Misc.title_to_event event with
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

let decode_token_params req =
  let after = get_unique_from_req "after" req in
  let before = get_unique_from_req "before" req in
  let groups = StringMap.find_opt "group" req.req_params in
  let min_ponderation = get_unique_from_req "min_level" req in
  let max_ponderation = get_unique_from_req "max_level" req in
  let confidential = get_unique_from_req "confidential" req in
  let tags = get_unique_from_req "tags"  req in
  let readonly = get_unique_from_req "readonly" req in
  let pretty = get_unique_from_req "pretty" req in
  let after = Option.bind after Misc.string_to_date in
  let before = Option.bind before Misc.string_to_date in
  let min_level =
    Option.map Int32.of_int @@ Option.bind min_ponderation int_of_string_opt
  in
  let max_level =
    Option.map Int32.of_int @@ Option.bind max_ponderation int_of_string_opt
  in
  let tags =
    Option.bind
      tags
      (fun str -> if str = "" then None else Some (String.split_on_char ',' str))
  in
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

let timeline_data (req, tid) _ () =
  Lwt_io.printl "CALL timeline_data" >>= (fun () ->
      let (
        start_date, end_date,
        min_ponderation, max_ponderation,
        tags, groups,
        confidential, _readonly,
        _pretty
      ) = decode_token_params req in
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
      end >>=
        function
        | Ok (Timeline {title; events; edition_rights}) ->
          (* Todo: parametrize the removal of categories *)
          let remove_category (i, e) = (i, {e with Data_types.group = None}) in
          let title, events =
            if edition_rights then
              title, events
            else
              Option.map remove_category title,
              List.map remove_category events in
          EzAPIServerUtils.return (Ok (Db_data.Timeline {title; events; edition_rights}))
        | other -> EzAPIServerUtils.return other
      )
    )

let remove_event (req, timeline_id) _ () =
  Lwt_io.printl "CALL remove_event" >>= (fun () ->
      match StringMap.find_opt "event_id" req.EzAPI.Req.req_params with
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

let register_user _ _ user =
  Lwt_io.printl "CALL register_user" >>= fun () ->
  EzAPIServerUtils.return (Writer.register_user user)

let create_timeline_lwt req auth title name public =
  match get_auth_email req with
  | None ->
    Writer.create_public_timeline title name
  | Some email ->
    if not public && auth then
      Writer.create_private_timeline email title name
    else
      Writer.create_public_timeline title name

let send_link ~lang ~readonly_tid ~email ~timeline_name ~admin_tid =
  let lang =
    match lang with
    | Some "en" -> Emails.En
    | Some "fr" -> Emails.Fr
    | _ -> Emails.En in
  let mail = Emails.creation_email ~lang ~readonly_tid ~email ~timeline_name ~admin_tid in
  Email.Sendgrid_xhr.send_base
    ~api_key:(Api_config.Sendgrid.key ())
    mail

let create_timeline (req, name) _ (title, public) =
  Lwt_io.printl "CALL create_database" >>= fun () ->
  let name' = Misc.trim name in
  is_auth req (fun auth ->
    match create_timeline_lwt req auth title name' public with
      | Error _ as e -> EzAPIServerUtils.return e
      | (Ok (admin_tid, readonly_tid)) as ok ->
        match StringMap.find_opt "email" req.EzAPI.Req.req_params with
        | None | Some [] ->
          Lwt_io.printl "No email provided, returning" >>= fun () ->
          EzAPIServerUtils.return ok
        | Some (email :: _) ->
          Lwt_io.printl ("Sending to " ^ email) >>= fun () ->
          let lang =
            Option.bind
              (StringMap.find_opt "lang" req.EzAPI.Req.req_params)
              Misc.hd_opt
          in
          send_link ~lang ~readonly_tid ~email ~timeline_name:name ~admin_tid >>=
          function
          | Error (id, msg) ->
            Lwt_io.printl
              (Format.asprintf
                 "Error %i: %a"
                 id
                 (Misc.pp_opt Format.pp_print_string)
                 msg) >>=
            fun () -> EzAPIServerUtils.return ok
          | Ok _ -> EzAPIServerUtils.return ok
    )

let import_timeline (req, name) _ (title, events, public) =
  Lwt_io.printl "CALL import_database" >>= fun () ->
  let name = Misc.trim name in
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
    match get_auth_email req with
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
    match get_auth_email req with
    | None ->
      unknown_error "[user_timelines] Error: email should be in params"
    | Some email -> EzAPIServerUtils.return @@ Writer.remove_user email
  )

let remove_timeline (req,tid) _ () =
  Format.eprintf "CALL remove_timeline";
  if_ ~error:unauthorized edition_rights (req,tid) (fun () ->
    EzAPIServerUtils.return @@ Writer.remove_timeline tid
  )

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
  let pretty = get_unique_from_req "pretty" req in
  EzAPIServerUtils.return @@ Writer.update_token_pretty ~pretty ~token tid

let update_token_readonly (req, token) _ tid =
  let readonly =
    match get_unique_from_req "readonly" req with
    | Some "false" -> false
    | _ -> true
  in
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
  match get_unique_from_req "pretty" req with
  | None -> EzAPIServerUtils.return (Error "No name given")
  | Some new_name ->
    EzAPIServerUtils.return @@ Writer.update_timeline_name new_name tid

(* Miscelaneous *)
let version _ _ () =
  Lwt_io.printl "CALL version" >>= fun () ->
  ok "0.1"

let login _ _ user =
  Lwt_io.printl "CALL login" >>= fun () ->
  Reader.Login.login user >>= function
  | None -> unauthorized ()
  | Some s -> ok s

let logout _ _ (email, cookie) =
  Lwt_io.printl "CALL logout" >>= fun () ->
  Reader.Login.logout email cookie >>= ok
