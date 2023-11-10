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

let full_data = "data.json"

let timeline_data = "data_raw.json"

let to_int_opt s =
  if s = "" then None
  else try Some (int_of_string s) with Invalid_argument _ -> None

let to_text headline text =
  {
    headline;
    text
  }

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

let to_title_event headline text = {
  start_date = None;
  end_date = None;
  text = {headline; text};
  group = None;
  media = None;
  ponderation = 0;
  confidential = false;
  unique_id = "timeline-title-" ^ (short_title headline);
  last_update = None;
  tags = []
}

let to_title line =
  match String.split_on_char '\t' line with
  | title :: [] -> to_title_event title ""
  | title :: text :: _ -> to_title_event title text
  | _ -> raise (Invalid_argument (Format.sprintf "Missing elements for building title (%s)" line))

let metaevent_to_event meta : event option =
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

let event_to_metaevent e = {
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

let opt f = function
  | None -> None
  | Some e -> Some (f e)


module StringSet = Set.Make (String)
module StringMap = Map.Make (String)
module IntMap = Map.Make (struct type t = int let compare = (-) end)

let fold_lefti f =
  let i = ref (-1) in
  let f = fun acc elt -> i := !i + 1; f !i acc elt in
  let rec loop acc = function
    | [] -> acc
    | hd :: tl -> loop (f acc hd) tl
  in loop

module Header = struct
  type t = int StringMap.t
  let header_to_map line : t=
    let headers = String.split_on_char '\t' line in
    let map =
      fold_lefti
        (fun i acc title -> StringMap.add title i acc)
        StringMap.empty
        headers
    in
    match StringMap.find_opt "Debut" map with
      None -> failwith "Field 'Debut' must be in the header"
    | Some _ -> map

  let get_elt name header data =
    match StringMap.find_opt name header with
      None -> None
    | Some i -> try Some (data.(i)) with Invalid_argument _ -> None

  
  
  let start_name        = "Start"
  let end_name          = "End"
  let typ_name          = "Group"
  let typ2_name         = "Tags"
  let importance_name   = "Ponderation"
  let media_name        = "Media"
  let confidential_name = "Confidential"
  let title_name        = "Title"
  let text_name         = "Narration"
  let unique_id_name    = "Unique Id"

  let names = [
    start_name; end_name;
    typ_name; typ2_name;
    importance_name; media_name;
    confidential_name; text_name;
    unique_id_name; title_name (* Let title at the end of this list *)
  ]
  
  let start_date   : t -> string array -> string option = get_elt start_name
  let end_date     : t -> string array -> string option = get_elt end_name
  let typ          : t -> string array -> string option = get_elt typ_name
  let typ2         : t -> string array -> string option = get_elt typ2_name
  let importance   : t -> string array -> string option = get_elt importance_name
  let media        : t -> string array -> string option = get_elt media_name
  let confidential : t -> string array -> string option = get_elt confidential_name
  let title        : t -> string array -> string option = get_elt title_name
  let text         : t -> string array -> string option = get_elt text_name
  let unique_id    : t -> string array -> string option = get_elt unique_id_name

  let pp fmt header =
    Format.fprintf fmt
      "%a"
      (Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.fprintf fmt ", ") (fun fmt (str, i) -> Format.fprintf fmt "%s -> %i" str i))
      (List.of_seq (StringMap.to_seq header))

  let str_header sep =
    Format.asprintf "%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt _ -> Format.fprintf fmt "%s" sep)
         (fun fmt -> Format.fprintf fmt "%s")) names
      
end

let pp_opt pp fmt = function
    None -> ()
  | Some e -> Format.fprintf fmt "%a" pp e

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

let fopt f = function
    None -> None
  | Some e -> f e

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

let min32 (i1 : Int32.t) (i2 : Int32.t) = 
 if i1 < i2 then i1 else i2

let max32 (i1 : Int32.t) (i2  : Int32.t) =
 if i1 > i2 then i1 else i2

let intersect_list l1 l2 =
  List.fold_left
    (fun acc e -> if List.mem e l1 then e :: acc else acc)
    []
    l2

let min_date d1 d2 =
  if CalendarLib.Date.compare d1 d2 < 0 then d1 else d2

let max_date d1 d2 =
  if CalendarLib.Date.compare d1 d2 > 0 then d1 else d2

let is_empty_list = function [] -> true | _ -> false

let pp_str_opt = pp_opt (fun fmt -> Format.fprintf fmt "%s")
