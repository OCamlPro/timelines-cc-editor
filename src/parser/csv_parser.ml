(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

let full_data = "data.json"

let () =
  let input_file =
    try Sys.argv.(1) with
      _ -> Format.printf "You must provide a data file"; exit 1 in
  let timeline = Utils.Csv_utils.from_file input_file in
  let json = Json_encoding.construct Data_encoding.timeline_encoding timeline in
  Data_encoding.write_json json full_data
