open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_utils
open Bootstrap_helpers.Grid
open Utils

open Data_types

let page_name = "home"

let back_button () =
  Ui_utils.simple_button
    "home-back"
    (fun _ -> ignore @@ !Dispatcher.dispatch ~path:page_name ~args:(Ui_utils.get_args ()))
    "Back"

let edit_button (events : (int * event) list) title categories =
    Ui_utils.split_button "timeline-page" 8 "Edit slide" "Cancel"
      ~action_at_split:(fun () ->
          match Ui_utils.get_split_from_splitted "timeline-page" with
          | None -> false
          | Some split -> begin
              let args = Ui_utils.get_args () in
              try
                (* We will now check in which context the button has been clicked:
                   was it while on the title page or on an event page ? *)
                let id = (* if id = 0, then it is the title *)
                  match List.assoc_opt "id" args with
                  | None -> begin
                      match title with
                      | None -> fst @@ List.hd @@ List.rev events (* The id of the first event *)
                      | Some _ -> 0
                    end
                  | Some id -> int_of_string id in
                if id = 0 then begin (* Title case *)
                  match title with
                  | None -> assert false (* Previous test checks that *)
                  | Some title ->
                    let form, get_title =
                      Admin.event_form
                        title
                        id
                        categories in
                    let add_button =
                      Ui_utils.simple_button
                        "edit-add"
                        (fun self ->
                           Controller.update_action
                             Admin.compare
                             args
                             id
                             categories
                             title
                             (get_title ())
                             (fun () ->
                                Js_utils.log "Going back to main page %s with %i arguments"
                                  page_name
                                  (List.length args)
                                ;
                                !Dispatcher.dispatch
                                  ~path:page_name
                                  ~args
                             )
                        )
                        "Update title" in
                    let split_content =
                      [form; add_button; back_button ()] in
                    Manip.replaceChildren split split_content; true
                end else begin
                  match List.assoc_opt id events with
                  | None ->
                    alert (Format.sprintf "Event %i not found" id);
                    raise (Invalid_argument "edit_button")
                  | Some event ->
                    let event = Utils.event_to_metaevent event in
                    let form, get_event =
                      Admin.event_form
                        event
                        id
                        categories in
                    let add_button =
                      Ui_utils.simple_button
                        "edit-add"
                        (fun self ->
                           Controller.update_action
                             Admin.compare
                             args
                             id
                             categories
                             event
                             (get_event ())
                             (fun () ->
                                Js_utils.log "Going back to main page %s with %i arguments"
                                  page_name
                                  (List.length args)
                                ;
                                !Dispatcher.dispatch
                                  ~path:page_name
                                  ~args
                             )
                        )
                        "Update event" in
                    let split_content =
                      [form; add_button; back_button ()] in
                    Manip.replaceChildren split split_content; true
                end
              with Invalid_argument s ->
                Js_utils.log "Error while splitting in edition: %s" s;
                false
            end
        )
      ~action_at_unsplit:(fun () -> true)

let id_current_event has_title (order : int list) =
  let args = Ui_utils.get_args () in
  match List.assoc_opt "id" args with
  | None -> begin
      if has_title then 0 else
      try
        let id = List.hd @@ List.rev order in
        Js_utils.log "Id of last event: %i" id;
        id
      with
        _ -> 0
    end
  | Some i -> try
      let res = int_of_string i in
      Js_utils.log "Current event id: %i" res;
      res
    with _ ->
      Js_utils.log "Error while searching for id, writing 0 instead";
      0

let display_timeline update_action is_auth args categories title (events : (int * event) list) :
  unit =
  let events = List.map (
      fun (i,e) ->
        let id = Ui_utils.slide_id_from_id i in
        {e with end_date = None; unique_id = id}) events in
  let cmd =
    let timeline = {events; title} in
    let json = Json_encoding.construct (Data_encoding.timeline_encoding) timeline in
    let yoj  = Json_repr.to_yojson json in
    let str  = Yojson.Safe.to_string yoj in
    let () = Js_utils.log "yojson to send: %s" str in
      Format.asprintf
        "window.timeline = new TL.Timeline('home-timeline-embed',%s)"
        str in
  let () = Js_of_ocaml.Js.Unsafe.js_expr cmd in
  ()

let form is_auth args categories =
  let form_with_content title key input_type =
    let content = List.assoc_opt key args in
    Ui_utils.placeholder ~id:key ?content ~title ~name:key ~input_type ()
  in
  let start_date, get_start_date = form_with_content "From" "start-date" (Other `Date) in
  let end_date,   get_end_date   = form_with_content "To"   "end-date"   (Other `Date) in
  let precision,  get_precision  =
    form_with_content "Precision" "max_level" (Number (Some 0, None)) in
  let user_view,  get_user_view =
    let test_user_view =
      match List.assoc_opt "confidential" args with
      | Some "false" -> true
      | _ -> false
    in
    let checkbox, getter =
      Ui_utils.dynamic_checkbox
        ~classes:[]
        ~style:"margin-top:5%"
        ~checked:test_user_view
        ~value:"user-view"
        ~id:"user-view"
        ~oncheck:(fun _ ->
            let args = Ui_utils.assoc_add_unique "confidential" "false" args in
            ignore @@ !Dispatcher.dispatch ~path:page_name ~args)
        ~onuncheck:(fun _ ->
            let args = Ui_utils.assoc_add_unique "confidential" "true" args in
            ignore @@ !Dispatcher.dispatch ~path:page_name ~args)
    in
    div ~a:[a_class [row]][
      div ~a:[a_class [clg3]] [txt "Public view"];
      div ~a:[a_class [clg3]] [checkbox]
    ], getter
  in
  let category_html, category_getters =
    let actual_categories = Ui_utils.assoc_list "group" args in
    List.split @@
    List.map
      (fun category ->
         let checked = List.mem category actual_categories in
         let checkbox, getter =
           Ui_utils.dynamic_checkbox
             ~classes:[]
             ~style:"margin-top:7%"
             ~checked
             ~value:"user-view"
             ~id:"user-view"
             ~oncheck:(fun _ ->
                 let args = Ui_utils.assoc_add "group" category args in
                 ignore @@ !Dispatcher.dispatch ~path:page_name ~args)
             ~onuncheck:(fun _ ->
                 let args = Ui_utils.assoc_remove_with_binding "group" category args in
                 ignore @@ !Dispatcher.dispatch ~path:page_name ~args)
         in
         div ~a:[a_class [row]][
           div ~a:[a_class [clg3]] [txt category];
           div ~a:[a_class [clg3]] [checkbox]
         ], getter
      )
      categories in
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
        let precision =
          match get_precision () with
          | None -> []
          | Some p -> ["max_level", p] in
        let confidential =
          if get_user_view () then
            ["confidential", "false"]
          else
            ["confidential", "true"]
        in
        let min_level =
          match List.assoc_opt "min_level" args with
          | None -> []
          | Some i -> ["min_level", i] in
        let categories =
          List.flatten @@
          List.map2
            (fun category get ->
               if get () then ["group", category] else []
            )
            categories
            category_getters
        in
        start_date @ end_date @ confidential @ min_level @ precision @ categories 
      in
      ignore @@ !Dispatcher.dispatch ~path:page_name ~args; true in
    div
      ~a:[
        a_class ["btn";"btn-primary"];
        a_onclick action
      ] [txt "Filter"];
  in form (
    (if is_auth then [user_view] else [])@
    [h4 [txt "Categories"]] @
    category_html @
    [h4 [txt "Other filters"]] @
    [start_date] @ [end_date] @ [precision] @
    [button]
  )

module EventPanelNoAuth = Panel.MakePageTable(
  struct
    let title_span _ = span []
    let table_class = "table"
    let page_size = 100
    let name = "events"
    let theads () =
      tr [
        th [txt "Date"];
        th [txt "Headline"];
      ]
  end
  )

module EventPanelAuth = Panel.MakePageTable(
  struct
    let title_span _ = span []
    let table_class = "table"
    let page_size = 100
    let name = "events"
    let theads () =
      tr ~a:[] [
        th ~a:[ a_style "width:25%" ]   [txt "Date"];
        th ~a:[ a_style "width:37.5%" ] [txt "Headline"];
        th ~a:[ a_style "width:12.5%" ] [txt "Ponderation"];
        th ~a:[ a_style "width:12.5%" ] [txt "Confidential"];
        th ~a:[ a_style "width:12.5%" ] [txt ""];
      ]
  end
  )

module type EventPanelType = module type of EventPanelNoAuth

let make_panel_lines (events : (int * event) list) =
  match events with
  | [] -> [ tr [ td ~a: [ a_colspan 9 ] [ txt "No event" ]]]
  | _ ->
    List.map
      (fun (id,{start_date; text = {headline; _}}) ->
         let onclick () =
           let args = Ui_utils.get_args () in
           let new_args = Ui_utils.assoc_add_unique "id" (string_of_int id) args in
           ignore @@ !Dispatcher.dispatch ~path:page_name ~args:new_args
         in
         tr ~a:[a_onclick (fun _ -> onclick (); true); a_class ["clickable"]] [
           td [txt @@ Format.asprintf "%a" (CalendarLib.Printer.Date.fprint "%d/%m/%Y") start_date];
           td [txt headline];
         ]
      )
      events

let make_panel_lines_auth (events : (int * event) list) =
  match events with
  | [] -> [ tr [ td ~a: [ a_colspan 9 ] [ txt "No event" ]]]
  | _ ->
    List.map
      (fun (id,{start_date; text = {headline; _}; ponderation; confidential; _}) ->
         let onclick () =
           let args = Ui_utils.get_args () in
           let new_args = Ui_utils.assoc_add_unique "id" (string_of_int id) args in
           ignore @@ !Dispatcher.dispatch ~path:page_name ~args:new_args
         in
         let a style = (a_style style) :: [
           a_onclick (fun _ -> onclick (); true);
         ] in
         let date =
           Format.asprintf "%a"
             (CalendarLib.Printer.Date.fprint "%d/%m/%Y") start_date in
         tr ~a:[a_class ["clickable"]] [
           td ~a:(a "width:25%") [txt date];
           td ~a:(a "width:37.5%") [txt headline];
           td ~a:(a "width:12.5%") [txt @@ string_of_int ponderation];
           td ~a:(a "width:12.5%") [txt @@ string_of_bool confidential];
           td [
             Ui_utils.simple_button
               ("edit-table-" ^ string_of_int id)
               (fun _ ->
                  !Dispatcher.dispatch
                    ~path:Admin.page_name
                    ~args:["action", "edit"; "id", string_of_int id]
               )
               "Edit"
           ]
         ]
      )
      events

let page
    is_auth args
    categories
    title
    (events : (int * event) list) =
  let (module EventPanel : EventPanelType) =
    if is_auth then
      (module EventPanelAuth)
    else (module EventPanelNoAuth) in
  let has_title = match title with None -> false | Some _ -> true in
  let events =
    List.sort
      (fun
        (_,{start_date = s1; end_date = e1; _})
        (_,{start_date = s2; end_date = e2; _}) ->
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
  let add_button, back_button =
    Ui_utils.split_button "timeline-add-home-button" 8 "Add new event" "Cancel"
      ~action_at_split:(fun () ->
          match Ui_utils.get_split_from_splitted "timeline-page" with
          | None -> false
          | Some split ->
            let form, get_event =
              Admin.add_new_event_form categories in
            let add_button =
              Ui_utils.simple_button
                "add-page"
                (fun _ ->
                   Controller.add_action (get_event ());
                   ignore @@ !Dispatcher.dispatch ~path:page_name ~args
                )
                "Add new event" in
            let split_content =
              [form; add_button; back_button ()] in
            Manip.replaceChildren split split_content; true)
      ~action_at_unsplit:(fun () -> true)
  in
  let admin_link =
    if is_auth then
      let user =
        match Ui_utils.Session.get_value "email" with
        | None -> ""
        | Some name -> name in
      div [
        div [txt ("Hello " ^ user)];
        div ~a:[a_class ["btn"; "btn-primary"];
                a_onclick (fun _ ->
                    ignore @@ !Dispatcher.dispatch ~path:"admin" ~args:[]; true)
               ] [txt "Admin page"];
        div ~a:[a_class ["btn"; "btn-primary"];
                a_onclick (fun _ -> Controller.logout (); true)
               ] [txt "Logout"];
        add_button; back_button
      ]
    else
      Admin.admin_page_login () in
  let default_page =
    div [
      div ~a:[a_id "timeline-page"][
        div ~a:[a_class [row]] [
          div ~a:[a_class [clg12];
                  a_id "home-timeline-embed";
                  a_style" height: 600px"][]
        ];
        div ~a:[a_class [row]] [
          div ~a:[a_class [clg3]] [admin_link];
          div ~a:[a_class [clg3]] [form is_auth args categories];
          div ~a:[a_class [clg6]] [EventPanel.make ~footer:true ()];
        ];
      ]
    ]
  in
  let table_elts =
    let make_lines =
      if is_auth then
        make_panel_lines_auth
      else
        make_panel_lines in
    make_lines events |> Array.of_list in
  let init () =
    display_timeline Controller.update_action is_auth args categories title events;
    (* Now, adding additional events to timeline links. *)
    let order =
      (* Requires a second reordering to use for the table and not for the timeline itself. *)
      let slides = Manip.by_class "tl-slide" in (* In the page order *)
      List.fold_left
        (fun acc elt ->
           try
             let id = Ocp_js.Js.to_string @@ (Manip.get_elt "id" elt)##.id in
             (Ui_utils.id_from_slide_id id) :: acc
           with _ -> acc
        ) [] slides
    in
    let () =
      let future_links = Manip.by_class "tl-timegroup-message" in
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
    in
    (* Going on the correct event *)
    let () =
      let goto_id =
        match List.assoc_opt "id" args with
        | None -> 0
        | Some i -> try int_of_string i with _ -> 0
      in
      let id_at_pos =
        let rec loop cpt = function
          | [] -> 0
          | hd :: tl ->
            if hd = goto_id then
              cpt
            else
              loop (cpt - 1) tl in
        let cpt_init =
          let len = List.length order in
          if has_title then len else len - 1 in
        loop cpt_init order
      in
      Js_utils.log "To go to next, %i steps are required" id_at_pos;
      Ui_utils.slide_event Next id_at_pos in
    (* Adding page change in URL *)
    let prev_event id events =
      let rec loop = function
        | [] -> assert false
        | [_] ->
          Js_utils.log "This is the first event";
          if has_title then Some 0 else None
        | i :: ((nxt :: _) as tl) ->
          if i = id then begin
            Some nxt
          end else begin
            loop tl
          end
      in
      loop events in
    let next_event id events =
      let events = List.rev events in
      if id = 0 then
        Some (List.hd events)
      else
        prev_event id events in
    let () = begin
      let push_next () =
        let current_id = id_current_event has_title order in
        match next_event current_id order with
        | None -> Js_utils.log "No next event"
        | Some new_id ->
          let new_args = Ui_utils.assoc_add_unique "id" (string_of_int new_id) args in
          let new_url = Ui_utils.url page_name new_args in
          Ui_utils.push new_url in
      let push_prev () =
        let current_id = id_current_event has_title order in
        match prev_event current_id order with
        | None -> Js_utils.log "No prev event"
        | Some new_id ->
          let new_args = Ui_utils.assoc_add_unique "id" (string_of_int new_id) args in
          let new_url = Ui_utils.url page_name new_args in
          Ui_utils.push new_url
      in
      let () =
        let next = Ui_utils.slide_changer Next in
        Ocp_js.Dom.addEventListener
          (Manip.get_elt "click" next)
          (Ocp_js.Dom.Event.make "click")
          (Ocp_js.Dom.handler (fun _ ->
               push_next ();
               Ocp_js.Js._true))
          Ocp_js.Js._true |> ignore in
      let () =
        let prev = Ui_utils.slide_changer Prev in
        Ocp_js.Dom.addEventListener
          (Manip.get_elt "click" prev)
          (Ocp_js.Dom.Event.make "click")
          (Ocp_js.Dom.handler (fun e ->
               push_prev ();
               Ocp_js.Js._true))
          Ocp_js.Js._true |> ignore in
      let () =
        let reinit = Ui_utils.slide_reinit () in
        Ocp_js.Dom.addEventListener
          (Manip.get_elt "click" reinit)
          (Ocp_js.Dom.Event.make "click")
          (Ocp_js.Dom.handler (fun e ->
               ignore @@ !Dispatcher.dispatch ~path:page_name ~args:(Ui_utils.assoc_remove "id" args);
               Ocp_js.Js._true))
          Ocp_js.Js._true |> ignore in
      let () = (* Adding links to timeline lower part *)
        let lower_links = List.rev @@ Manip.by_class "tl-timemarker" in
        try
          Js_utils.log "Events ordered: %i ; Lower links: %i" (List.length order) (List.length lower_links);
          List.iter2
            (fun i elt ->
               Ocp_js.Dom.addEventListener
                 (Manip.get_elt "click" elt)
                 (Ocp_js.Dom.Event.make "click")
                 (Ocp_js.Dom.handler (fun e ->
                      let url =
                        Ui_utils.url page_name (Ui_utils.assoc_add_unique "id" (string_of_int i) args) in
                      Ui_utils.push url;
                      Ocp_js.Js._true))
                 Ocp_js.Js._true |> ignore
            )
            order
            lower_links
        with Invalid_argument s -> Js_utils.log "Error in lower part listeners: %s" s
      in

      let top_buttons =
        let edit_buttons = 
          if is_auth then begin
            let edit_button, cancel_button = edit_button events title categories in
            [div [edit_button; cancel_button]]
          end else [] in 
        let export_vertical = [
          div
            ~a:[
              a_class ["btn"; "btn-primary"];
              a_id "export-pdf";
              a_onclick (fun _ -> Ui_utils.html2pdf ~align:V "slides-pdf"; true)
            ] [txt "Raw Timeline (Vertical)"]
        ] in 
        let export_hori = [
          div
            ~a:[
              a_class ["btn"; "btn-primary"];
              a_id "export-pdf";
              a_onclick (fun _ -> Ui_utils.html2pdf ~align:H "slides-pdf"; true)
            ] [txt "Raw Timeline (Horizontal)"]
        ] in
        div ~a:[a_class ["row"]; a_style "position: sticky; z-index:10; height:0px;  top: 0"]
          (edit_buttons @ export_vertical @ export_hori) in
      let timeline = Js_utils.find_component "page-content" in
      match Manip.children timeline with
      | [] -> Manip.appendChild timeline top_buttons
      | before :: _ -> Manip.appendChild ~before timeline top_buttons
    end
    in
    EventPanel.paginate_all
      ~urlarg_page:"" ~urlarg_size:"" table_elts;
    ignore (Js_of_ocaml.Js.Unsafe.eval_string
              "jQuery('[data-toggle=\"popover\"]').popover();")
    (* Initalize table *)
  in
  default_page, init
