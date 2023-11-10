(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

(** Hardcoded config file name. *)
let config_file_name = "db_config"
(* Todo: clean with proper option. *)

let database = ref "ocptl_db"
let host = ref None
let port = ref None
let user = ref None
let password = ref None

let encoding =
  Json_encoding.(
    obj5
      (opt "database" string)
      (opt "host" string)
      (opt "port" int)
      (opt "user" string)
      (opt "password" string)
  )

let init () =
  if Sys.file_exists config_file_name then
    try
      let ic = open_in config_file_name in
      let json = Ezjsonm.from_channel ic in
      close_in ic;
      let (cdatabase, chost, cport, cuser, cpassword) =
        Json_encoding.destruct encoding json
      in
      let (=:=) r v =
        match v with None -> () | Some v -> r := v in
      database =:= cdatabase;
      host := chost;
      port := cport;
      user := cuser;
      password := cpassword;
    with
    | exn ->
      Format.eprintf "Fatal error while reading %S:\n  %s\n%!"
        config_file_name (Printexc.to_string exn);
      exit 2
  else
    Format.eprintf
      "Config file %s does not exist, setting default DB parameters."
      config_file_name

let database () = !database
let host () = !host
let port () = !port
let user () = !user
let password () = !password
