(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

open Data_types

let raw_data = "data"

let timeline_data = "data_raw.json"

let short_title str =
  let allowed_char c =
    match c with
    | 'A' .. 'Z'
    | 'a' .. 'z'
    | '0' .. '9'
    | '.' | '-' | '_' -> c
    | _ -> '-'
  in
  if str = "" then
    "__no_title__id__"
  else
    let short =
      String.sub str 0 (min (String.length str) 60) in
    String.map allowed_char short

let to_title_event text = {
  start_date = None;
  end_date = None;
  text;
  group = None;
  media = None;
  ponderation = 0;
  confidential = false;
  unique_id = "timeline-title-" ^ (short_title text.headline);
  last_update = None;
  tags = []
}

let title_to_event meta : event option =
  match meta.start_date with
  | None -> None
  | Some start_date -> Some {
      start_date;
      end_date = meta.end_date;
      text = meta.text;
      media = meta.media;
      group = meta.group;
      confidential = meta.confidential;
      ponderation = meta.ponderation;
      unique_id = meta.unique_id;
      last_update = meta.last_update;
      tags = meta.tags
    }

let event_to_title e = {
  start_date = Some e.start_date;
  end_date = e.end_date;
  text = e.text;
  media = e.media;
  group = e.group;
  confidential = e.confidential;
  ponderation = e.ponderation;
  unique_id = e.unique_id;
  last_update = e.last_update;
  tags = e.tags
}

let to_date year month day =
  let default_first = function
      None -> 1
    | Some e -> e in
  let month = default_first month in
  let day   = default_first day   in
  CalendarLib.Date.make year month day

let string_to_date str =
  try
    match String.split_on_char '-' str with
    | [] -> None
    | [year] -> Some (to_date (int_of_string year) None None)
    | [year; month] -> Some (to_date (int_of_string year) (Some (int_of_string month)) None)
    | year :: month :: day :: _ ->
      Some (
        to_date
          (int_of_string year)
          (Some (int_of_string month))
          (Some (int_of_string day))
      )
  with Failure _ (* int_of_string *) -> None

let to_media url = {url}

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)
module IntMap = Map.Make (struct type t = int let compare = (-) end)

let pp_opt = Format.pp_print_option

let pp_event fmt (e : event) =
  Format.fprintf fmt
    "{start_date = %a;\n\
     end_date = %a;\n\
     text = %s; %s;\n\
     media = %a;\n\
     group = %a\n\
     confidential = %b\n\
     ponderation = %i;\n\
     last_update = %a;\n\
     tags = %a;\n\
     unique_id = %s;}"
    (CalendarLib.Printer.Date.fprint "%D") e.start_date
    (pp_opt (CalendarLib.Printer.Date.fprint "%D")) e.end_date
    e.text.headline e.text.text
    (pp_opt (fun fmt m -> Format.fprintf fmt "%s" m.url)) e.media
    (pp_opt (fun fmt   -> Format.fprintf fmt "%s")) e.group
    e.confidential
    e.ponderation
    (pp_opt (CalendarLib.Printer.Date.fprint "%D")) e.last_update
    (Format.pp_print_list
       ~pp_sep:(fun fmt _ -> Format.fprintf fmt ", ")
       (fun fmt -> Format.fprintf fmt "%s")) e.tags
    e.unique_id

let pp_title fmt (e : title) =
  Format.fprintf fmt
    "{start_date = %a;\n\
     end_date = %a;\n\
     text = %s; %s;\n\
     media = %a;\n\
     group = %a\n\
     confidential = %b\n\
     ponderation = %i;\n\
     last_update = %a\n\
     tags = %a;\n\
     unique_id = %s;}"
    (pp_opt (CalendarLib.Printer.Date.fprint "%D")) e.start_date
    (pp_opt (CalendarLib.Printer.Date.fprint "%D")) e.end_date
    e.text.headline e.text.text
    (pp_opt (fun fmt m -> Format.fprintf fmt "%s" m.url)) e.media
    (pp_opt (fun fmt   -> Format.fprintf fmt "%s")) e.group
    e.confidential
    e.ponderation
    (pp_opt (CalendarLib.Printer.Date.fprint "%D")) e.last_update
    (Format.pp_print_list
       ~pp_sep:(fun fmt _ -> Format.fprintf fmt ", ")
       (fun fmt -> Format.fprintf fmt "%s")) e.tags
    e.unique_id

let hd_opt = function
    [] -> None
  | hd :: _ -> Some hd

let list_init n f =
  let rec aux n acc =
    if n < 0 then raise (Invalid_argument "Utils.list_init")
    else if n = 0 then acc
    else aux (n - 1) (f (n - 1) :: acc)
  in
  aux n []

let trim =
  String.map
    (function | ' ' | '\n' | '\r' | '\t' -> '-' | c -> c)

let check_unique_id check_is_used id =
  let id = trim id in
  if check_is_used id then
    let rec loop (i : int) =
      let new_name = (id ^ "-" ^ (string_of_int i)) in
      if check_is_used new_name then
        loop (i + 1)
      else new_name
    in loop 2
  else id

let intersect_list l1 l2 =
  List.fold_left
    (fun acc e -> if List.mem e l1 then e :: acc else acc)
    []
    l2

let min_date d1 d2 =
  if CalendarLib.Date.compare d1 d2 < 0 then d1 else d2

let max_date d1 d2 =
  if CalendarLib.Date.compare d1 d2 > 0 then d1 else d2
