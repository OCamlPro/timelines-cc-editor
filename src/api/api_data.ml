(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

open Json_encoding
open Db_data

(* Redefinition of simple encodings for web *)
let unit = obj1 (req "unit" unit)

let string = obj1 (req "string" string)

let title_api_result_encoding : ((int * Timeline_data.Data_types.title) option) Json_encoding.encoding =
  option (tup2 int Data_encoding.title_encoding)

let events_api_result_encoding =
  list (
  tup2
    int
    Data_encoding.event_encoding
  )

let timeline_result_encoding =
  tup3
    title_api_result_encoding
    events_api_result_encoding
    bool

let timeline_data_api_result_encoding =
  union [
    case
     timeline_result_encoding
     (function
        | Timeline {title; events; edition_rights} -> Some (title, events, edition_rights)
        | _ -> None)
     (fun (title, events, edition_rights) -> Timeline {title; events; edition_rights});
     case
       unit
       (function NoTimeline -> Some () | _ -> None)
       (fun () -> NoTimeline)
  ]

(* Updates require a "Modified" case *)
type 'start_date update_meta_event_res =
  | Success
  | Modified of 'start_date Timeline_data.Data_types.meta_event option

let update_meta_event_res_encoding start_encoding =
  union [
    case
      (option (Data_encoding.meta_event_encoding start_encoding))
      (function Modified e -> Some e | _ -> None) (fun e -> Modified e);
    case unit
      (function Success -> Some () | _ -> None)
      (fun _ -> Success );
  ]

type update_event_res = CalendarLib.Date.t update_meta_event_res
type update_title_res = CalendarLib.Date.t option update_meta_event_res

let update_event_res_encoding =
  update_meta_event_res_encoding Data_encoding.date_encoding
let update_title_res_encoding =
  update_meta_event_res_encoding (Data_encoding.(option date_encoding))

let bool_of_kind = function
  | View -> true
  | Edit -> false

let kind_of_bool b =
  if b then View else Edit

let filter_encoding =
  conv
    (fun {timeline; kind; after; before; min_level; max_level;
          pretty; categories; tags; confidential_rights} ->
         (timeline, bool_of_kind kind, after, before, min_level, max_level,
          pretty, categories, tags, confidential_rights))
    (fun (timeline, kind, after, before, min_level, max_level,
          pretty, categories, tags, confidential_rights) ->
         {timeline; kind = kind_of_bool kind; after; before; min_level; max_level;
          pretty; categories; tags; confidential_rights})
    (obj10
       (req "timeline" string)
       (req "kind" bool)
       (opt "after" Data_encoding.date_encoding)
       (opt "before" Data_encoding.date_encoding)
       (opt "min_level" int32)
       (opt "max_level" int32)
       (opt "pretty" string)
       (opt "categories" (list string))
       (opt "tags" (list string))
       (req "confidential" bool)
    )

let admin_token = obj1 (req "admin" string)

let any_token = obj1 (req "token" string)

let update_event_encoding =
  obj4
    (req "event_id" int)
    (req "old" Data_encoding.title_encoding)
    (req "new" Data_encoding.title_encoding)
    (req "timeline_id" Json_encoding.string)

let create_timeline_output_encoding =
  obj2
    (req "admin_tid" Json_encoding.string)
    (req "readonly_tid" Json_encoding.string)
