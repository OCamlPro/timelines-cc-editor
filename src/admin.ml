open Js_utils
open Ocp_js
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Data_types
open Bootstrap_helpers
open Grid
open Form

type 'a input_type =
    Radio of string list
  | Other of 'a

let placeholder
    ?(classes=[])
    ?(content="")
    ?(input_type= Other `Text)
    ?title
    ~id
    ~name
    () =
  let to_form form =
    match title with
      None -> form
    | Some elt -> txt elt :: form
  in
  match input_type with
  | Radio l ->
    div (
      to_form @@
      List.flatten @@
      List.map
        (fun str ->
           [
             input
               ~a:[
                 a_id id;
                 a_class (["placeholder"; form_inline] @ classes);
                 a_value str;
                 a_input_type `Radio;
                 a_name name
               ] ();
             label [txt str]
           ]
        )
        l
    )
  | Other t ->
    div (
      to_form @@ [
        input
          ~a:[a_id id;
              a_class (["placeholder"; form_inline] @ classes);
              a_value content;
              a_input_type t
             ] ()
      ]
    )

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
let valid       i = "button-"      ^ i

let event_form (e: event) (id_line: int) =
  let idl = string_of_int id_line in
  form
    ~a:[
      a_id ("line-" ^ idl);
      a_class ["row"; "line"];
      a_action "update_form.php";
      a_method `Post
    ] [
    placeholder
      ~id:(start_year idl)
      ~content:(string_of_int e.start_date.year)
      ~title:"Start year"
      ~name:"start_year"
      ();
    (let content = match e.start_date.month with None -> "" | Some e -> string_of_int e in
     placeholder
       ~id:(start_month idl)
       ~content
       ~title:"Start month"
       ~name:"start_month"
       ());
    (let content = match e.end_date with None -> "" | Some e -> string_of_int e.year in
     placeholder
       ~id:(end_year idl)
       ~content
       ~title:"End year"
       ~name:"end_year"
       ());
    (let content =
       match e.end_date with Some {month = Some e; _} -> string_of_int e | _ -> "" in
     placeholder
       ~id:(end_month idl)
       ~content
       ~title:"End month"
       ~name:"end_month"
       ());
    placeholder
      ~id:(title idl)
      ~content:e.text.headline
      ~title:"Headline"
       ~name:"headline"
      ();
    (let content = match e.media with None -> "" | Some e -> e.url in
     placeholder
       ~id:(media idl)
       ~content
       ~title:"Media"
       ~name:"media"
       ());
    placeholder
      ~id:(group idl)
      ~classes:[clg1]
      ~content:(To_json.type_to_str e.group)
      ~title:"Group"
      ~name:"group"
      ~input_type:(Radio ["Software"; "Person"; "Client"])
      ();
    placeholder
      ~id:(text idl)
      ~content:e.text.text
      ~title:"Text"
      ~name:"text"
      ();
    placeholder
      ~content:"Save modifications"
      ~id:(valid idl)
      ~name:"submit"
      ~input_type:(Other `Submit)
      ()
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

let empty_event_form id =
  let empty_event = {
    start_date = {year = 0; month = None};
    end_date = None;
    text = {text = ""; headline = ""};
    media = None;
    group = None
  }
  in
  event_form empty_event id

let update_timeline_data page =
  To_json.read_json Utils.full_data
    (fun events ->
       let content = List.mapi (fun i e -> event_form e i) events.events in
       Manip.replaceChildren page content;
       Lwt.return (Ok "ok")
    )

let () =
   let path_str =
     match Jsloc.url () with
       Http h | Https h -> h.hu_path_string
     | _ -> "" in
   Js_utils.log "Path: %s"
     path_str;
   if path_str = "admin"
   then
     let page =
       match Manip.by_id "page-content" with
       | None -> assert false
       | Some s -> s in
   ignore @@ update_timeline_data page
