open Js_of_ocaml_tyxml.Tyxml_js.Html

let page_name = ""

let display_timeline json : unit =
  let json = Format.sprintf "{\"events\":%s}" json in
  let cmd =
    Format.asprintf "window.timeline = new TL.Timeline('timeline-embed',%s)" json in
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


