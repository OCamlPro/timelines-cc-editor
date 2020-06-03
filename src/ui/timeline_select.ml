open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_utils
open Bootstrap_helpers.Grid

let timeline_div timeline =
  div
    ~a:[a_class ["timeline-case"; clg4]] [
    div ~a:[a_class ["timeline-name"]]      [txt timeline];
    div ~a:[a_class ["btn"; "btn-primary"]; a_id ("select-button-"^timeline)] [txt "Select"];
  ]

let new_timeline_button () =
  Ui_utils.split_button "timeline-page" 6 "Create a new timeline" "Cancel"
    ~action_at_split:(fun () ->
        match Ui_utils.get_split_from_splitted "timeline-page" with
        | None -> false
        | Some split -> begin
            let form, get_title = Admin.empty_event_form [] in
            let add_button =
              Ui_utils.simple_button
                "edit-add"
                (fun self ->
                   let title = get_title () in
                   Controller.create_timeline title (function
                       | Ok timeline -> 
                         !Dispatcher.dispatch
                           ~path:"home"
                           ~args:["timeline", timeline]
                       | Error s ->
                         Lwt.return (Ok (Js_utils.alert s))
                     )
                )
                "Update event" in
            let back_button = 
              Ui_utils.simple_button
                "back-from-edit"
                (fun _ -> 
                   match Manip.by_id "timeline-page-unsplit" with
                   | None -> () 
                   | Some b -> Ui_utils.click b
                ) 
                "Back" in
            let split_content =
              [form; add_button; back_button] in
            Manip.replaceChildren split split_content; true
          end
      )
    ~action_at_unsplit:(fun () -> true)

let timelines_list timelines =
  let rec loop = function
    | [] -> []
    | hd :: [] -> [timeline_div hd]
    | hd :: hd' :: [] -> [timeline_div hd; timeline_div hd']
    | hd1 :: hd2 :: hd3 :: tl ->
      timeline_div hd1 ::
      timeline_div hd2 :: 
      timeline_div hd3 :: loop tl
  in
  let ok, cancel = new_timeline_button () in  
  div ((div [ok; cancel]) :: loop timelines)
    
  
