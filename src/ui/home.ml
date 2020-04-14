open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_utils
open Bootstrap_helpers.Grid
open Utils

open Data_types

let page_name = ""

let add_button_to_event i event =
  let button = (* todo: not a string html element *)
    Format.sprintf
      "<a href='admin?action=edit&id=%i' class=\"btn btn-light row\"> Edit </a>"
      i
  in
  let new_text =
    let text = event.text.text in
    Format.asprintf "<div>%s</div>\n<div>%s</div>"
      text
      button
  in
  {event with text = {headline = event.text.headline; text = new_text}}

let process_events is_auth events =
  List.map (fun (i, event) ->
      if is_auth then
        add_button_to_event i event
      else event
    )
    events

let display_timeline is_auth (events : (int * event) list) : unit =
  let events = process_events is_auth events in
  let cmd =
    let json = Json_encoding.construct (Json_encoding.list Data_encoding.event_encoding) events in
    let yoj = Json_repr.to_yojson json in
    Format.asprintf
      "window.timeline = new TL.Timeline('timeline-embed',{\"events\":%s})"
      (Yojson.Safe.to_string yoj) in
  Js_utils.log "Cmd = %s" cmd;
  let () = Js_of_ocaml.Js.Unsafe.js_expr cmd in
  (* Now, adding links *)
  let future_links = Manip.by_class "tl-timegroup-message" in
  let () =
    List.iter
      (fun future_link ->
         let children = Manip.children future_link in
         match children with
         | [] -> ()
         | elt :: _ -> (* There should only be 1 element *)
           let node = Ocp_js.Html.toelt elt in
           match Ocp_js.Js.Opt.to_option node##.nodeValue with
           | None -> ()
           | Some name ->
             let name = Ocp_js.Js.to_string name in
             let link = Ui_utils.a_link ~args:["group", name] ~path:page_name [txt name] in
             Manip.replaceChildren future_link [link]
      )
      future_links
  in ()

let form is_auth args =
  let form_with_content title key input_type =
    let content = List.assoc_opt key args in
    Ui_utils.placeholder ~id:key ?content ~title ~name:key ~input_type ()
  in
  let start_date, get_start_date = form_with_content "From" "start-date" (Other `Date) in
  let end_date,   get_end_date   = form_with_content "To"   "end-date"   (Other `Date) in
  let user_view,  get_user_view =
    let test_user_view =
      match Ui_utils.Session.get_value "user-view" with
        None -> false
      | Some _ -> true
    in
    form_with_content "User view" "user-view" (Checkbox test_user_view) 
  in
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
        let confidential =
          match get_user_view () with
          | Some "true" -> ["confidential", "false"]
          | Some "false" -> ["confidential", "true"]
          | _ -> [] in
        start_date @ end_date @ confidential
      in
      ignore @@ !Dispatcher.dispatch ~path:page_name ~args; true in
    div
      ~a:[
        a_class ["btn";"btn-primary"];
        a_onclick action
      ] [txt "Filter"];
  in form (
    [start_date] @ [end_date] @
    (if is_auth then [user_view] else [])@
    [button]
  )

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
  let events =
    List.sort
      (fun
        {start_date = s1; end_date = e1; _}
        {start_date = s2; end_date = e2; _} ->
        let cmp = CalendarLib.Date.compare s2 s1 in
        if cmp = 0 then
          match e1, e2 with
          | None, None -> 0
          | Some _, None -> 1
          | None, Some _ -> -1
          | Some d1, Some d2 -> CalendarLib.Date.compare d2 d1
        else cmp
      )
      events
  in
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

let page is_auth args events =
  let page =
    div ~a:[a_class [row]] [
      div ~a:[a_class [clg3]] [form is_auth args];
      div ~a:[a_class [clg9]] [EventPanel.make ~footer:true ()];
    ]
  in
  let table_elts =
    let events = snd @@ List.split events in
    make_panel_lines events |> Array.of_list in
  let init () =
    display_timeline is_auth events;
    EventPanel.paginate_all
      ~urlarg_page:"" ~urlarg_size:"" table_elts;
    ignore (Js_of_ocaml.Js.Unsafe.eval_string
              "jQuery('[data-toggle=\"popover\"]').popover();")
    (* Initalize table *)
  in
  page, init
