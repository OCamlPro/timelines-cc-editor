open Js_utils
open Ocp_js
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Data_types
open Bootstrap_helpers
open Grid

let placeholder ?content ?(classes=[]) ~id () =
  let content =
    match content with
    | None -> []
    | Some t -> [txt t]
  in
  div ~a:[a_id id; a_class ["placeholder"]] content

let add_text_to_placeholder id t =
  match Manip.by_id id with
    None -> failwith "better error mesg"
  | Some s -> Manip.replaceChildren s [txt t]

let get_value id =
  match Manip.by_id id with
    None -> ""
  | Some elt -> Manip.value elt

let start_year  i = "start-year-"  ^ i
let start_month i = "start-month-" ^ i
let end_year    i = "end-year-"    ^ i
let end_month   i = "end-month-"   ^ i
let title       i = "title-"       ^ i
let media       i = "media-"       ^ i
let group       i = "group-"       ^ i
let text        i = "text-"        ^ i

let event_line (e: event) (id_line: int) =
  let idl = string_of_int id_line in
  div ~a:[a_id ("line-" ^ idl); a_class ["row"; "line"]] [
    placeholder
      ~id:(start_year idl)
      ~classes:[clg1]
      ~content:(string_of_int e.start_date.year) ();
    (let content = match e.start_date.month with None -> "" | Some e -> string_of_int e in
     placeholder
       ~id:(start_month idl)
       ~classes:[clg1]
       ~content ());
    (let content = match e.end_date with None -> "" | Some e -> string_of_int e.year in
     placeholder
       ~id:(end_year idl)
       ~classes:[clg1]
       ~content ());
    (let content =
       match e.end_date with Some {month = Some e; _} -> string_of_int e | _ -> "" in
     placeholder
       ~id:(end_month idl)
       ~classes:[clg1]
        ~content ());
    placeholder ~id:(title idl) ~content:e.text.headline ();
    (let content = match e.media with None -> "" | Some e -> e.url in
     placeholder
       ~id:(media idl)
       ~classes:[clg1]
       ~content ());
    placeholder
      ~id:(group idl)
      ~classes:[clg1]
      ~content:(To_json.type_to_str e.group)();
    placeholder
      ~id:(text idl)
      ~classes:[clg5]
      ~content:e.text.text ();
    div [txt "Todo: edit button"]
  ]

let read_line (id_line: int) =
  let idl = string_of_int id_line in
  let start_date =
    let year  = int_of_string @@ get_value @@ start_year  idl in
    let month = try Some (int_of_string @@ get_value @@ start_month idl) with _ -> None in
    { year; month } in
  let end_date =
    match int_of_string_opt @@ get_value @@ start_year  idl with
      None -> None
    | Some year ->
      let month = try Some (int_of_string @@ get_value @@ start_month idl) with _ -> None in
      Some {year; month} in
  let text =
    let text = get_value @@ text idl in
    let headline = get_value @@ title idl in
    {text; headline} in
  let media = try Some {url = get_value @@ media idl} with _ -> None in
  let group = To_json.to_type @@ get_value @@ group idl in {
    start_date; end_date; text; media; group
  }

let empty_event_line id =
  let empty_event = {
    start_date = {year = 0; month = None};
    end_date = None;
    text = {text = ""; headline = ""};
    media = None;
    group = None
  }
  in
  event_line empty_event id

let timeline_data () =
  let events = To_json.read_json Utils.full_data in
  List.mapi (fun i e -> event_line e i) events.events

let () =
   let path_str =
     match Jsloc.url () with
       Http h | Https h -> h.hu_path_string
     | _ -> "" in
   if path_str = "admin"
   then
     let page =
       match Manip.by_id "page-content" with
       | None -> assert false
       | Some s -> s in
     Manip.replaceChildren page (timeline_data ())
