(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

val init : string -> unit
val port : unit -> int
val host : unit -> string

module Sendgrid :
sig
  val init : string -> unit
  val key : unit -> string
  val from : unit -> string
  val from_alias : unit -> string option
end
