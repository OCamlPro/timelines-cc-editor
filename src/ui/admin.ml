open Js_utils
open Ocp_js
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Data_types
open Ui_utils
open Bootstrap_helpers
open Grid

let page_name = "admin"

let start_date   i = "start-date-"  ^ i
let end_date     i = "end-date-"    ^ i
let title        i = "title-"       ^ i
let media        i = "media-"       ^ i
let group        i = "group-"       ^ i
let text         i = "text-"        ^ i
let unique_id    i = "unique-id-"   ^ i
let confidential i = "confid-"      ^ i
let ponderation  i = "ponderation-" ^ i
let valid        i = "button-"      ^ i

let back_button () =
  Ui_utils.simple_button
    "admin-back"
    (fun _ -> ignore @@ !Dispatcher.dispatch ~path:page_name ~args:[])
    "Back"

let event_form
    ?(readonly = false)
    (e: date option meta_event)
    (id_line: int) categories =
  let idl =
    let num = string_of_int id_line in
    if readonly then
      num ^ "--readonly"
    else num in
  let start_date, get_start_date =
    let str_date =
      Format.asprintf "%a" (Utils.pp_opt (CalendarLib.Printer.Date.fprint "%F")) e.start_date
    in
    placeholder
      ~readonly
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
      ~readonly
      ~id:(end_date idl)
      ~content
      ~title:"To"
      ~name:"end_date"
      ~input_type:(Other `Date)
      () in

  let headline, get_headline =
    placeholder
      ~readonly
      ~id:(title idl)
      ~content:e.text.headline
      ~title:"Headline"
       ~name:"headline"
       () in

  let unique_id, get_unique_id =
    placeholder
      ~readonly
      ~id:(unique_id idl)
      ~content:e.unique_id
      ~title:"Unique id"
      ~name:"unique-id"
       () in

  let media, get_media =
    let content = match e.media with None -> "" | Some e -> e.url in
    placeholder
      ~readonly
      ~id:(media idl)
      ~content
      ~title:"Media"
      ~name:"media"
      () in

  let group, get_group =
    let str_group = match e.group with None -> "" | Some g -> g in
    placeholder
      ~readonly
      ~id:(group idl)
      ~title:"Group"
      ~name:"group"
      ~input_type:(Radio (str_group, categories))
      () in

  let text, get_text =
    placeholder
      ~readonly
      ~id:(text idl)
      ~content:e.text.text
      ~title:"Text"
      ~name:"text"
      ~input_type:TextArea
      ~style:"width: 300px; height: 200px; resize: both"
      () in

  let confidential, get_confidential =
    placeholder
      ~readonly
      ~id:(confidential idl)
      ~title:"Confidential"
      ~name:"confidential"
      ~input_type:(Checkbox e.confidential)
      () in

  let ponderation, get_ponderation =
    placeholder
      ~readonly
      ~id:(ponderation idl)
      ~title:"Ponderation"
      ~name:"ponderation"
      ~content:(string_of_int e.ponderation)
      ~input_type:(Number (Some 0, None))
      () in
  
  let get_event () =
    let start_date =
      match get_start_date () with
      | None -> None
      | Some d -> Utils.string_to_date d
    in
    let end_date =
      match get_start_date () with
      | None -> None
      | Some d -> Utils.string_to_date d in

    let confidential =
      match get_confidential () with
      | None | Some "false" -> false
      | Some "true" -> true
      | Some thingelse ->
        let error =
          Format.sprintf "Confidentiality must be 'true' or 'false': %s is invalid" thingelse
        in failwith error in

    let ponderation =
      match get_ponderation () with
      | None -> 0
      | Some i -> try int_of_string i with _ ->
        failwith ("Ponderation must be an integer: " ^ i ^ " is invalid") in

    let title = get_headline () in

    let unique_id =
      match get_unique_id () with
      | Some i -> i
      | None ->
        match title with
        | None -> failwith "You must either provide a unique ID or a title"
        | Some t -> Utils.short_title t
    in
    Data_encoding.to_event
      ~start_date
      ~end_date
      ~typ:(get_group ())
      ~confidential
      ~ponderation
      ~media:(get_media ())
      ~title
      ~text:(get_text ())
      ~typ2:None
      ~unique_id

  in
  let html =
    form
      ~a:[
        a_id ("line-" ^ idl);
        a_class ["line"];
      ] (
      [
        start_date;
        end_date;
        media;
        headline;
        unique_id;
        group;
        text;
        ponderation;
        confidential;
      ]
    ) in
  html, get_event

let empty_event_form id action =
  let empty_event = {
    start_date = Some (CalendarLib.Date.today ());
    end_date = None;
    text = {text = ""; headline = ""};
    media = None;
    group = None;
    confidential = false;
    ponderation = 0;
    unique_id = ""
  }
  in
  event_form empty_event id action

let event_short_row (i, event) =
  let stri = string_of_int i in
  let start_year = string_of_int @@ CalendarLib.Date.year event.start_date in
  let edit_link =
    Ui_utils.a_link
      ~path:"admin"
      ~args:["action", "edit"; "id", stri]
      ~classes:["btn"; "btn-primary"; row]
      [txt "Edit"]
  in
  div ~a:[a_class [row]] [
    div ~a:[a_class [clg1]] [txt @@ string_of_int i];
    div ~a:[a_class [clg1]] [txt @@ start_year];
    div ~a:[a_class [clg3]] [txt event.text.headline];
    div ~a:[a_class [clg2]] [edit_link]
  ]

let events_list args events =
  let add_link =
    div ~a:[
      a_class ["btn"; "btn-primary"];
      a_onclick (fun _ ->
          ignore @@ !Dispatcher.dispatch ~path:"admin" ~args:["action", "add"]; true)
    ] [txt "Create event"] in
  let logout =
    div ~a:[a_class ["btn"; "btn-primary"];
            a_onclick (fun _ -> Controller.logout (); true)]
      [txt "Logout"] in
  let export =
    div ~a:[a_class ["btn"; "btn-primary"];
            a_onclick (fun _ -> Controller.export_database args; true)]
      [txt "Export database"] in
  let back_to_home =
    div ~a:[a_class ["btn"; "btn-primary"];
            a_onclick (fun _ ->
                ignore @@ !Dispatcher.dispatch ~path:"home" ~args:[]; true)] [txt "Home"] in

  (div ~a:[a_class [row]] [add_link; logout; export; back_to_home]) ::
  List.map event_short_row events

let add_new_event_form categories =
  empty_event_form
    0
    categories

let rec compare
    (id : int)
    (categories : string list)
    (old_event : date option meta_event option)
    (new_event : date option meta_event) =
  let prefix, old_event, new_event =
    match old_event with
    | Some event -> (* The event has been modified *)
      let prefix =
        txt "The event has been modified while you were editing it. \
             Please check for conflicts before resubmitting" in
      let old_event = [
        fst @@ event_form ~readonly:true event id categories
      ] in
      let new_event =
        let form, get_new_event = event_form new_event id categories in
        let args = Ui_utils.get_args () in
        let update_button =
          Ui_utils.simple_button
            "compare-update"
            (fun _ ->
               Controller.update_action compare args id categories event (get_new_event ())
                 (fun () ->
                    Js_utils.log "Event updated";
                    !Dispatcher.dispatch
                      ~path:page_name
                      ~args:[]
                 )
            )
            "Update event"
        in [
          form;
          update_button;
          back_button ()
        ]
      in prefix, old_event, new_event
    | None -> (* The event has been deleted *)
      let prefix =
        txt "The event has been modified while you were editing it. \
             Chech again your event before resubmitting it." in
      let old_event = [] in
      let new_event =
        let form, get_new_event = event_form new_event id categories in
        let add_button =
          Ui_utils.simple_button
            "comapre-add"
            (fun _ -> Controller.add_action (get_new_event ()))
            "Add new event"
        in [form; add_button; back_button ()] in
      prefix, old_event, new_event
  in
  div [prefix;
       div ~a:[a_class [row]] [
         div ~a:[a_class [clg6]] old_event;
         div ~a:[a_class [clg6]] new_event
       ]
      ]

(* Login utilities *)

let admin_page_login () =
  let login, get_login =
    placeholder
      ~id:"login"
      ~title:"Login"
      ~name:"login"
      ~newline:true
      () in

  let pwd, get_pwd =
    placeholder
      ~id:"pwd"
      ~title:"Password"
      ~name:"password"
      ~input_type:(Other `Password)
      ~newline:true
      () in

  let login_button =
    div
      ~a:[
        a_class ["btn";"btn-primary"];
        a_onclick
          (fun _ ->
             match get_login (), get_pwd () with
             | Some login, Some pwd ->
               Controller.login login pwd;
               true
             | _ ->
               Js_utils.log "Login or password not found";
               false
          )
      ] [txt "Login"] in

  let register_button =
    div
      ~a:[
        a_class ["btn";"btn-primary"];
        a_onclick
          (fun _ ->
             match get_login (), get_pwd () with
             | Some login, Some pwd ->
               Controller.register_account login pwd;
               true
             | _ ->
               Js_utils.log "Missing data@.";
               false
          )
      ] [txt "Register"] in
  form [
    login;
    pwd;
    login_button;
    register_button
  ]
