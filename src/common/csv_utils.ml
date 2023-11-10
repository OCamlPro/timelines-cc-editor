(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

exception Not_an_event of string

let tag_separator = ','

let aspf pp = Format.asprintf "\"%a\"" pp
let spf = Format.sprintf "\"%s\""

let not_an_event fmt = Format.kasprintf (fun s -> raise (Not_an_event s)) fmt

let header =
  ["Start"; "End"; "Headline"; "Text"; "Media";
   "Group"; "Confidential"; "Ponderation"; "Unique Id";
   "Last updated"; "Tags"]

let quote_to_doublequote str =
  if str = "" then "" else
  let quoted, str =
    match str.[0], str.[String.length str - 1] with
      '"', '"' -> true, String.sub str 1 ((String.length str) - 2)
    | _ -> false, str
  in
  let new_string =
    let l = String.split_on_char '"' str in
    String.concat "\"\"" l
  in
  if quoted then
    "\"" ^ new_string ^ "\""
  else new_string

let pp_date = (CalendarLib.Printer.Date.fprint "%Y-%m-%d")

let meta_event_to_csv_line pp_start
    Data_types.{
      start_date;
      end_date;
      text;
      media;
      group;
      confidential;
      ponderation;
      unique_id;
      last_update;
      tags
    } : string list = [
    aspf pp_start start_date;
    aspf (Utils.pp_opt pp_date) end_date;
    spf (quote_to_doublequote text.headline);
    spf (quote_to_doublequote text.text);
    aspf (Utils.pp_opt (fun fmt m -> Format.pp_print_string fmt m.Data_types.url)) media;
    aspf (Utils.pp_opt Format.pp_print_string) group;
    string_of_bool confidential;
    string_of_int ponderation;
    spf unique_id;
    aspf (Utils.pp_opt pp_date) last_update;
    Format.asprintf
      "\"%a\""
      (Format.pp_print_list
         ~pp_sep:(fun fmt _ -> Format.pp_print_char fmt tag_separator)
         Format.pp_print_string)
      tags
  ]

let title_to_csv_line = meta_event_to_csv_line (Utils.pp_opt pp_date)

let event_to_csv_line = meta_event_to_csv_line pp_date

let csv_line_to_meta_event uids = function
    start_date :: end_date ::
    headline :: text ::
    media :: group ::
    confidential :: ponderation ::
    unique_id :: rest ->
    let start_date = Utils.string_to_date start_date in
    let end_date = Utils.string_to_date end_date in
    let text =
      Data_types.{headline; text} in
    let media = if media = "" then None else Some Data_types.{url = media} in
    let group = if group = "" then None else Some group in
    let confidential = try bool_of_string confidential with _ -> true in
    let ponderation = try int_of_string ponderation with _ -> 0 in
    let unique_id =
      let id =
        match unique_id with
        | "" -> headline
        | _ -> unique_id
      in
      Utils.check_unique_id (fun s -> List.mem s uids) id
    in
    let last_update, rest =
      match rest with
        [] -> None, []
      | d :: rest -> Utils.string_to_date d, rest in

    let tags =
      match rest with
      | [] -> []
      | t :: _ -> String.split_on_char tag_separator t in Data_types.{
    start_date;
    end_date;
    text;
    media;
    group;
    confidential;
    ponderation;
    unique_id;
    last_update;
    tags
  }
  | l -> not_an_event "Event has %i fields: incorrect." (List.length l)

(* Todo: check Csv lib for a specific printer. *)
let to_string (c : Csv.t) : string =
  Format.asprintf "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt _ -> Format.fprintf fmt "\n")
       (Format.pp_print_list
          ~pp_sep:(fun fmt _ -> Format.fprintf fmt ",")
          (fun fmt -> Format.fprintf fmt "%s"))) (header :: c)

let from_chan chan =
  let title =
    try
      Some (csv_line_to_meta_event [] @@ Csv.next chan)
    with
      End_of_file -> None
  in
  let events =
    let elist = ref [] in
    let uids =
      match title with
      | None -> ref []
      | Some title -> ref [title.unique_id] in
    try
      while true do
        let l = Csv.next chan in
        let me = csv_line_to_meta_event !uids l in
        match Utils.title_to_event me with
        | None ->
          not_an_event "Event (%a) is a title, expected an event." Utils.pp_title me
        | Some e ->
          elist := e :: !elist;
          uids  := e.unique_id :: !uids
      done;
      assert false
    with
    | End_of_file -> !elist
  in
  Data_types.{title; events}

let from_string str =
  from_chan
    (Csv.of_string
       ~separator:',' ~has_header:true
       ~backslash_escape:true ~excel_tricks:true
       str)

let from_file filename =
  let chan = open_in filename in
  let res = from_chan
      (Csv.of_channel
         ~separator:',' ~has_header:true
         ~backslash_escape:true ~excel_tricks:true
         chan)
  in
  close_in chan;
  res
