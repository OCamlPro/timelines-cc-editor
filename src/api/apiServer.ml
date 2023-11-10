(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

module H = ApiHandlers
module S = Api_services.ApiServices

let start () =
  let dir =
    EzAPIServerUtils.empty (*
    |> EzAPIServerUtils.register S.event  H.event
    |> EzAPIServerUtils.register S.events H.events
    |> EzAPIServerUtils.register S.title H.title *)
    |> EzAPIServerUtils.register S.add_event H.add_event
    |> EzAPIServerUtils.register S.update_event H.update_event
    |> EzAPIServerUtils.register S.timeline_data H.timeline_data
    |> EzAPIServerUtils.register S.remove_event H.remove_event
    |> EzAPIServerUtils.register S.categories H.categories
    |> EzAPIServerUtils.register S.register_user H.register_user
    |> EzAPIServerUtils.register S.login H.login
    |> EzAPIServerUtils.register S.logout H.logout
    |> EzAPIServerUtils.register S.is_auth H.is_auth
    |> EzAPIServerUtils.register S.has_admin_rights H.has_admin_rights
    |> EzAPIServerUtils.register S.export_database H.export_database
    |> EzAPIServerUtils.register S.create_timeline H.create_timeline
    |> EzAPIServerUtils.register S.import_timeline H.import_timeline
    |> EzAPIServerUtils.register S.user_timelines H.user_timelines
    |> EzAPIServerUtils.register S.allow_user H.allow_user
    |> EzAPIServerUtils.register S.timeline_users H.timeline_users
    |> EzAPIServerUtils.register S.remove_user H.remove_user
    |> EzAPIServerUtils.register S.remove_timeline H.remove_timeline
    |> EzAPIServerUtils.register S.create_token H.create_token
    |> EzAPIServerUtils.register S.update_token_pretty H.update_token_pretty
    |> EzAPIServerUtils.register S.update_token_readonly H.update_token_readonly
    |> EzAPIServerUtils.register S.update_token H.update_token
    |> EzAPIServerUtils.register S.remove_token H.remove_token
    |> EzAPIServerUtils.register S.get_tokens H.get_tokens
    |> EzAPIServerUtils.register S.timeline_name H.timeline_name
    |> EzAPIServerUtils.register S.update_timeline_name H.update_timeline_name
    |> EzAPIServerUtils.register S.version H.version
  in
  let servers = [ Api_config.port (), EzAPIServerUtils.API dir ] in
  Lwt_main.run (
    Printexc.record_backtrace true;
    Printf.eprintf "Starting RPC servers on ports %s\n%!"
      (String.concat ","
         (List.map (fun (port,_) ->
              string_of_int port) servers));
    EzServer.server servers
  )
