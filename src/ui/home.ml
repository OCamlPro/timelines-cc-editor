open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_utils
open Bootstrap_helpers.Grid
open Utils

open Data_types

let page_name = "home"

let back_button () =
  Ui_utils.simple_button
    (fun () -> ignore @@ !Dispatcher.dispatch ~path:page_name ~args:[])
    "Back"

let add_button_to_event i event =
  let button = (* todo: not a string html element *)
    Format.sprintf
      "<a href='admin?action=edit&id=%i' class=\"btn btn-light row\"> Edit </a>"
      i
  in
  let new_text =
    let text = event.text.text in
    Format.asprintf "<div>%s</div>\n%s"
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

let id_current_event (events : (int * event) list) =
  let args = Ui_utils.get_args () in
  match List.assoc_opt "id" args with
  | None -> begin
      try
        let id = fst @@ List.hd @@ List.rev events in
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

let display_timeline is_auth args (events : (int * event) list) : unit =
  let events' = process_events is_auth events in
  let cmd =
    let json = Json_encoding.construct (Json_encoding.list Data_encoding.event_encoding) events' in
    let yoj = Json_repr.to_yojson json in
    Format.asprintf
      "window.timeline = new TL.Timeline('timeline-embed',{\"events\":%s})"
      (Yojson.Safe.to_string yoj) in
  Js_utils.log "Cmd = %s" cmd;
  let () = Js_of_ocaml.Js.Unsafe.js_expr cmd in
  (* Now, adding links *)
 ()

let form is_auth args categories =
  let form_with_content title key input_type =
    let content = List.assoc_opt key args in
    Ui_utils.placeholder ~id:key ?content ~title ~name:key ~input_type ()
  in
  let start_date, get_start_date = form_with_content "From" "start-date" (Other `Date) in
  let end_date,   get_end_date   = form_with_content "To"   "end-date"   (Other `Date) in
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
        let confidential =
          if get_user_view () then
            ["confidential", "false"]
          else
            ["confidential", "true"]
        in
        let categories =
          List.flatten @@
          List.map2
            (fun category get ->
               if get () then ["group", category] else []
            )
            categories
            category_getters
        in
        start_date @ end_date @ confidential @ categories
      in
      ignore @@ !Dispatcher.dispatch ~path:page_name ~args; true in
    div
      ~a:[
        a_class ["btn";"btn-primary"];
        a_onclick action
      ] [txt "Filter by date"];
  in form (
    (if is_auth then [user_view] else [])@
    [h4 [txt "Categories"]] @
    category_html @
    [h4 [txt "Dates"]] @
    [start_date] @ [end_date] @
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
           td [txt @@ Format.asprintf "%a" (CalendarLib.Printer.Date.fprint "%D") start_date];
           td [txt headline];
         ]
      )
      events

let page
    ~(login_action : string -> string -> unit)
    ~(logout_action : (string * string) list -> unit)
    ~(register_action : string -> string -> unit)
    ~(add_action : event -> unit)
    is_auth args categories (events : (int * event) list) =
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
  let events_in_timeline_order =
    (* Requires a second reordering to use for the table and not for the timeline itself. *)
    let rec loop all_acc local_start_date local_end_date local_acc = function
      | [] -> (local_acc :: all_acc) |> List.rev |> List.flatten
      | (i,hd) :: tl -> begin
          if (Some hd.start_date) = local_start_date && hd.end_date = local_end_date then
            loop all_acc local_start_date local_end_date ((i,hd) :: local_acc) tl
          else
            loop (local_acc :: all_acc) (Some hd.start_date) hd.end_date [i,hd] tl
        end
    in
    loop [] None None [] events
  in
  let add_button, back_button =
    Ui_utils.split_button "timeline-page" 8 "Add new event" "Cancel"
      ~action_at_split:(fun () ->
          match Ui_utils.get_split_from_splitted "timeline-page" with
          | None -> false
          | Some split ->
            let form, get_event =
              Admin.add_new_event_form categories in
            let add_button =
              Ui_utils.simple_button
                (fun () ->
                   add_action (get_event ());
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
                a_onclick (fun _ -> logout_action args; true)
               ] [txt "Logout"];
        add_button; back_button
      ]
    else
      Admin.admin_page_login ~login_action ~register_action in
  let default_page =
    div [
      div ~a:[a_id "timeline-page"][
        div ~a:[a_class [row]] [
          div ~a:[a_class [clg12];
                  a_id "timeline-embed";
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
    make_panel_lines events_in_timeline_order |> Array.of_list in
  let init () =
    display_timeline is_auth args events;
    (* Now, adding additional events to timeline links. *)
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
          | (hd,_) :: tl ->
            if hd = goto_id then
              cpt
            else
              loop (cpt - 1) tl in
        loop (List.length events_in_timeline_order - 1) events_in_timeline_order
      in
      Js_utils.log "To go to next, %i steps are required" id_at_pos;
      Ui_utils.slide_event Next id_at_pos in
    (* Adding page change in URL *)
    let prev_event id events =
      let rec loop = function
        | [] -> assert false
        | [_] -> Js_utils.log "This is the last event"; None
        | (i,_) :: (((nxt,_) :: _) as tl) ->
          if i = id then begin
            Some nxt
          end else begin
            loop tl
          end
      in loop events in
    let next_event id events = prev_event id (List.rev events) in
    let () = begin
      let push_next () =
        let current_id = id_current_event events_in_timeline_order in
        match next_event current_id events_in_timeline_order with
        | None -> Js_utils.log "No next event"
        | Some new_id ->
          let new_args = Ui_utils.assoc_add_unique "id" (string_of_int new_id) args in
          let new_url = Ui_utils.url page_name new_args in
          Ui_utils.push new_url in
      let push_prev () =
        let current_id = id_current_event events_in_timeline_order in
        match prev_event current_id events_in_timeline_order with
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
          List.iter2
            (fun (i, _) elt ->
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
            events_in_timeline_order
            lower_links
        with Invalid_argument s -> Js_utils.log "Error in lower part listeners: %s" s
      in ()
    end in
    EventPanel.paginate_all
      ~urlarg_page:"" ~urlarg_size:"" table_elts;
    ignore (Js_of_ocaml.Js.Unsafe.eval_string
              "jQuery('[data-toggle=\"popover\"]').popover();")
    (* Initalize table *)
  in
  default_page, init
