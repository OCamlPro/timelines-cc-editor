(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

open Timeline_data
open Data_types

let date_encoding =
  Json_encoding.(
    conv
      (fun date ->
         (
           CalendarLib.Date.year date,
           CalendarLib.Date.(int_of_month @@ month date),
           Some (CalendarLib.Date.(day_of_month date))
         ))
      (fun (year, month, day) -> Utils.to_date year (Some month) day)
      (obj3
         (req "year" int)
         (req "month" int)
         (opt "day" int)
      )
  )

let text_encoding = Json_encoding.(
    conv
      (fun {headline; text} -> (headline, text))
      (fun (headline, text) -> {headline; text})
      (obj2
         (req "headline" string)
         (req "text" string)))

let media_encoding =
  Json_encoding.(
    conv
      (fun {url} -> url)
      (fun url -> {url})
      (obj1 (req "url" string))
  )

let group_encoding = Json_encoding.string

let meta_event_encoding start_date_encoding =
  Json_encoding.(
    conv
      (fun
        {start_date; end_date;
         text; group;
         media; ponderation;
         confidential; unique_id;
         last_update; tags} ->
        (start_date, end_date,
         text, group,
         media, ponderation,
         confidential, unique_id,
         last_update, tags)
      )
      (fun
        (start_date, end_date,
         text, group,
         media, ponderation,
         confidential, unique_id,
         last_update, tags) ->
        {start_date; end_date;
         text; group;
         media; ponderation;
         confidential; unique_id;
         last_update; tags})
      (obj10
         (req "start_date"   start_date_encoding)
         (opt "end_date"     date_encoding)
         (req "text"         text_encoding)
         (opt "group"        group_encoding)
         (opt "media"        media_encoding)
         (req "ponderation"  int)
         (req "confidential" bool)
         (req "unique_id"    string)
         (opt "last_update"  date_encoding)
         (req "tags"         (list string))
      )
  )

let event_encoding = meta_event_encoding date_encoding

let title_encoding = meta_event_encoding (Json_encoding.option date_encoding)

let timeline_encoding =
  Json_encoding.(
    conv
      (fun {events; title} -> (events, title))
      (fun (events, title) -> {events; title})
      (obj2
         (req "events" (list event_encoding))
         (opt "title" title_encoding))
  )

let write_json json f =
  let chan = open_out f in
  let str = Format.asprintf "%a" (Json_repr.pp (module Json_repr.Ezjsonm)) json in
  output_string chan str;
  close_out chan
