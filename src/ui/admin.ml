open Js_utils
open Ocp_js
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Data_types
open Bootstrap_helpers
open Grid
open Form

type 'a input_type =
    Radio of string * string list
  | TextArea
  | Other of 'a

let placeholder
    ?(classes=[])
    ?(content="")
    ?(input_type= Other `Text)
    ?title
    ?(style="")
    ~id
    ~name
    () : [> Html_types.div ] Js_of_ocaml_tyxml.Tyxml_js.Html.elt * (unit -> string option) =
  let to_form form =
    match title with
      None -> form
    | Some elt -> div ~a:[a_class [clg3]] [txt elt] :: form
  in
  let row = div ~a:[a_class ["row"]] in
  let html_elt =
    match input_type with
    | Radio (value, l) ->
      let value = String.lowercase_ascii value in
      row (
        to_form @@
        List.flatten @@
        List.map
          (fun str ->
             let a =
               let default = [
                 a_id id;
                 a_class (["placeholder"; form_inline] @ classes);
                 a_value str;
                 a_input_type `Radio;
                 a_name name;
                 a_style style;
               ] in
               if String.(equal (lowercase_ascii str) value) then
                 a_checked () :: default
               else default in [
               input ~a ();
               label [txt str]
             ]
          )
          l
      )
    | TextArea ->
      row (
        to_form @@ [
          textarea
            ~a:[a_id id;
                a_class (["placeholder"] @ classes);
                a_style style;
               ] (txt content)
        ]
      )
    | Other t ->
      row (
        to_form @@ [
          input
            ~a:[a_id id;
                a_class (["placeholder"] @ classes);
                a_value content;
                a_input_type t;
                a_style style;
               ] ()
        ]
      )
  in
  let getter =
    fun () ->
      match Manip.by_id id with
      | None -> Js_utils.log "Error: placeholder %s not found" id; None
      | Some e -> Some (Manip.value e) in
  html_elt, getter

let add_text_to_placeholder id t =
  match Manip.by_id id with
    None -> failwith "better error mesg"
  | Some s -> Manip.replaceChildren s [txt t]

let get_value id =
  match Manip.by_id id with
    None -> ""
  | Some elt -> Manip.value elt

let start_date  i = "start-date-"  ^ i
let end_date    i = "end-date-"    ^ i
let title       i = "title-"       ^ i
let media       i = "media-"       ^ i
let group       i = "group-"       ^ i
let text        i = "text-"        ^ i
let valid       i = "button-"      ^ i

let event_form (e: event) (id_line: int) action =
  let idl = string_of_int id_line in
  let start_date, get_start_date =
    let str_date =
      Format.asprintf "%a" (CalendarLib.Printer.Date.fprint "%F") e.start_date
    in
    placeholder
      ~id:(start_date idl)
      ~content:str_date
      ~title:"From"
      ~name:"start_date"
      ~input_type:(Other `Date)
      () in

  let end_date, get_end_date =
    let content =
       match e.end_date with
       | None -> ""
       | Some d ->
         Format.asprintf "%a" (CalendarLib.Printer.Date.fprint "%F") d in
    placeholder
      ~id:(end_date idl)
      ~content
      ~title:"To"
      ~name:"end_date"
      ~input_type:(Other `Date)
      () in

  let headline, get_headline =
    placeholder
      ~id:(title idl)
      ~content:e.text.headline
      ~title:"Headline"
       ~name:"headline"
       () in

  let media, get_media =
    let content = match e.media with None -> "" | Some e -> e.url in
    placeholder
      ~id:(media idl)
      ~content
      ~title:"Media"
      ~name:"media"
      () in

  let group, get_group =
    let str_group = match e.group with None -> "" | Some g -> g in
    placeholder
      ~id:(group idl)
      ~title:"Group"
      ~name:"group"
      ~input_type:(Radio (str_group, ["Software"; "Person"; "Client"; "OCaml"]))
      () in

  let text, get_text =
    placeholder
      ~id:(text idl)
      ~content:e.text.text
      ~title:"Text"
      ~name:"text"
      ~input_type:TextArea
      ~style:"width: 300px; height: 200px; resize: both"
      () in
  let get_event () =
    let start_date =
      match get_start_date () with
      | None -> assert false
      | Some str_date -> begin
          match Utils.string_to_date str_date with
          | None -> begin
              Js_utils.log "Error with date %s" str_date;
              failwith "Error while parsing date"
            end
          | Some date -> date end
    in
    let end_date =
      match get_start_date () with
      | None -> None
      | Some d -> Utils.string_to_date d in
    Data_encoding.to_event
      start_date
      end_date
      (get_group ())
      None
      None
      (get_media ())
      (get_headline ())
      (get_text ())
  in
  form
    ~a:[
      a_id ("line-" ^ idl);
      a_class ["line"];
    ] [
    start_date;
    end_date;
    media;
    headline;
    group;
    text;
    div
      ~a:[
        a_class ["btn";"btn-primary"; row];
        a_onclick
          (fun _ ->
             action (get_event ());
             true
          )
      ] [txt "Update timeline"];
    br ();
    div ~a:[a_class ["btn"; "btn-primary"; row]] [
      a ~a:[a_href (Ui_utils.link ""); ] [txt "Back"]
    ]
  ]

let empty_event_form id action =
  let empty_event = {
    start_date = Utils.to_date 1 None None;
    end_date = None;
    text = {text = ""; headline = ""};
    media = None;
    group = None
  }
  in
  event_form empty_event id action

let event_short_row i event =
  let stri = string_of_int (i + 1) in
  let start_year = string_of_int @@ CalendarLib.Date.year event.start_date in
  let edit_link =
    a ~a:[a_href (Ui_utils.link ~args:["action", "edit"; "id", stri] "admin");
          a_class ["button"]] [txt "Edit"] in
  div ~a:[a_class [row]] [
    div ~a:[a_class [clg1]] [txt @@ string_of_int i];
    div ~a:[a_class [clg1]] [txt @@ start_year];
    div ~a:[a_class [clg3]] [txt event.text.headline];
    div ~a:[a_class [clg2]] [edit_link]
  ]

let events_list events = List.mapi event_short_row events

let add_new_event_form action = empty_event_form 0 action
