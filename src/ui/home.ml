open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_utils
open Bootstrap_helpers.Grid

open Data_types

let page_name = ""

let add_button_to_event i event =
  let button = (* todo: not a string html element *)
    Format.sprintf
      "<a href='admin?action=edit&id=%i' class=\"btn btn-light row\"> Edit </a>" i in
  let new_text =
    let text = event.text.text in
    Format.asprintf "<div>%s</div>\n<div>%s</div>"
      text
      button
  in
  {event with text = {headline = event.text.headline; text = new_text}}

let process_events events =
  List.map
    (fun (i, event) -> add_button_to_event i event)
    events

let display_timeline (events : (int * event) list) : unit =
  let events = process_events events in
  let cmd =
    let json = Json_encoding.construct (Json_encoding.list Data_encoding.event_encoding) events in
    let yoj = Json_repr.to_yojson json in
    Format.asprintf
      "window.timeline = new TL.Timeline('timeline-embed',{\"events\":%s})"
      (Yojson.Safe.to_string yoj) in
  Js_utils.log "Cmd = %s" cmd;
  Js_of_ocaml.Js.Unsafe.js_expr cmd

let form args =
  let form_with_content title key input_type =
    let content = List.assoc_opt key args in
    Ui_utils.placeholder ~id:key ?content ~title ~name:key ~input_type ()
  in
  let start_date, get_start_date = form_with_content "From" "start-date" (Other `Date) in
  let end_date,   get_end_date   = form_with_content "To"   "end-date"   (Other `Date) in
  let button =
    let action _ =
      let args =
        let start_date =
          match get_start_date () with
          | None -> []
          | Some d -> ["start_date", d] in
        let end_date =
          match get_end_date () with
          | None -> []
          | Some d -> ["end_date", d] in
        start_date @ end_date
      in
      ignore @@ !Dispatcher.dispatch ~path:page_name ~args; true in
    div
      ~a:[
        a_class ["btn";"btn-primary"];
        a_onclick action
      ] [txt "Filter"];
    in form [start_date; end_date; button]

module EventPanel = Panel.MakePageTable(
  struct
    let title_span _ = span []
    let table_class = "default-table"
    let page_size = 20
    let name = "events"
    let theads () =
      tr [
        th [txt "Date"];
        th [txt "Headline"];
      ]
  end
  )

let make_panel_lines events =
  match events with
  | [] -> [ tr [ td ~a: [ a_colspan 9 ] [ txt "No event" ]]]
  | _ ->
    List.map
      (fun {start_date; text = {headline; _}} ->
         tr [
           td [txt @@ Format.asprintf "%a" (CalendarLib.Printer.Date.fprint "%D") start_date];
           td [txt headline];
         ]
      )
      events

let page args events =
  let page =
    div ~a:[a_class [row]] [
      div ~a:[a_class [clg3]] [form args];
      div ~a:[a_class [clg9]] [EventPanel.make ~footer:true ()];
    ]
  in
  let table_elts =
    let events = snd @@ List.split events in
    make_panel_lines events |> Array.of_list in
  let init () =
    display_timeline events;
    EventPanel.paginate_all
      ~urlarg_page:"" ~urlarg_size:"" table_elts;
    ignore (Js_of_ocaml.Js.Unsafe.eval_string
              "jQuery('[data-toggle=\"popover\"]').popover();")
    (* Initalize table *)
  in
  page, init
