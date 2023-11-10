(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

val short_title : string -> string

val to_title_event : Data_types.text -> Data_types.title

val title_to_event : Data_types.title -> Data_types.event option

val event_to_title : Data_types.event -> Data_types.title

val to_date : int -> int option -> int option -> Data_types.date

val string_to_date : string -> Data_types.date option

val to_media : string -> Data_types.media

module StringSet : Set.S with type elt = String.t
module StringMap : Map.S with type key = String.t
module IntMap : Map.S with type key = int

val pp_opt :
  ?none:(Format.formatter -> unit -> unit) ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit

val pp_event : Format.formatter -> Data_types.event -> unit

val pp_title : Format.formatter -> Data_types.title -> unit

val hd_opt : 'a list -> 'a option

val fopt : ('a -> 'b option) -> 'a option -> 'b option

val list_init : int -> (int -> 'a) -> 'a list

val trim : string -> string

val check_unique_id :
  (string -> bool) -> string -> string

val intersect_list : 'a list -> 'a list -> 'a list

val min_date :
  ([< CalendarLib.Period.date_field ] as 'a) CalendarLib.Date.date ->
  'a CalendarLib.Date.date -> 'a CalendarLib.Date.date

val max_date :
  ([< CalendarLib.Period.date_field ] as 'a) CalendarLib.Date.date ->
  'a CalendarLib.Date.date -> 'a CalendarLib.Date.date
