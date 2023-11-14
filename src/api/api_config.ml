(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

let api_port = ref 13579
let api_host = ref "timelines.cc"

let encoding =
  Json_encoding.(
    obj2
      (opt "api_port" int)
      (opt "api_host" string)
  )

let init file =
  try
    let ic = open_in file in
    let json = Ezjsonm.from_channel ic in
    close_in ic;
    let api_port', api_host' =
      Json_encoding.destruct encoding json
    in
    let (=:=) r v =
      match v with None -> () | Some v -> r := v in
    api_port =:= api_port';
    api_host =:= api_host';
  with
  | exn ->
    Printf.eprintf "Fatal error while reading %S:\n  %s\n%!"
      file (Printexc.to_string exn);
    exit 2

let port () = !api_port
let host () = !api_host

module Sendgrid = struct
  let key : string ref = ref ""
  let from : string ref = ref ""
  let from_alias : string option ref = ref None

  let encoding =
    Json_encoding.(
      obj3
        (req "key" string)
        (req "from" string)
        (opt "from_alias" string)
    )

  let init file =
    try
      Format.printf "Reading sendgrid config file %s@." file;
      let ic = open_in file in
      let json = Ezjsonm.from_channel ic in
      close_in ic;
      let key', from', from_alias' = Json_encoding.destruct encoding json in
      key := key';
      from := from';
      from_alias := from_alias'
    with
    | exn ->
      Printf.eprintf "Fatal error while reading %S:\n  %s\n%!"
        file (Printexc.to_string exn);
      exit 2

  let key () = !key
  let from () = !from
  let from_alias () = !from_alias
end
