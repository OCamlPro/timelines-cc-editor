(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

(* Main *)

let api_config : string option ref = ref None
let sendgrid_config : string option ref = ref None

let args = [
  "--api-config",
  Arg.String (fun s -> api_config := Some s),
  "API configuration file";

  "--sendgrid-config",
  Arg.String (fun s -> sendgrid_config := Some s),
  "Sendgrid configuration file";
]

let load_api_config () =
  match !api_config with
  | None -> ()
  | Some f -> Config.API.init f

let load_sendgrid_config () =
  match !sendgrid_config with
  | None -> ()
  | Some f -> Config.Sendgrid.init f

let () =
  let () = Arg.parse args (fun s -> Format.printf "%s" s) "Usage msg TODO" in
  load_api_config ();
  load_sendgrid_config ();
  Api_lib.ApiServer.start ()
