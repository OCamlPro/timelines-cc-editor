(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

val date_encoding :
  CalendarLib.Period.date_field CalendarLib.Date.date
    Json_encoding.encoding

val event_encoding : Data_types.event Json_encoding.encoding

val title_encoding : Data_types.title Json_encoding.encoding

val timeline_encoding : Data_types.timeline Json_encoding.encoding

(** Writes the JSON in a file. *)
val write_json : Json_repr.ezjsonm -> string -> unit
