(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

exception Not_an_event of string

(** Transforms a title into a CSV line. *)
val title_to_csv_line : Data_types.title -> string list

(** Same, but for events. *)
val event_to_csv_line : Data_types.event -> string list

(** Returns a CSV as a string. *)
val to_string : Csv.t -> string

(** From a CSV string, returns the timeline title and events.
    Raises Not_an_event if a line does not correspond to an event. *)
val from_string : string -> Data_types.timeline

(** Same, but for a file. *)
val from_file : string -> Data_types.timeline
