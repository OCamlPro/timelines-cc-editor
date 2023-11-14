(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

(** Returns the database name. *)
val database : unit -> string

(** Returns the host, if set. *)
val host : unit -> string option

(** Returns the port, if set. *)
val port : unit -> int option

(** Returns the user, if set. *)
val user : unit -> string option

(** Returns the database password, if set. *)
val password : unit -> string option

(** Initialize the database configuration with the content of 'db_config'. *)
val init : unit -> unit
