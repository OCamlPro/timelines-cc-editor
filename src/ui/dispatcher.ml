(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

let dispatch : (
  path: string ->
  args : (string * string) list ->
  (unit, string) result Lwt.t) ref = (* Set in pages.ml *)
  ref (fun ~path:_ ~args:_ -> assert false)
