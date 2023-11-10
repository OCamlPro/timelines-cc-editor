(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

open Ui_common

let () =
  Lang.init (fun () ->
      let path = Ui_utils.get_path () in
      ignore @@ Pages.dispatch ~path ~args:(Ezjs_loc.args ())
    )
