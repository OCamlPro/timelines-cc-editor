open Data_types
open Js_utils
open Js_of_ocaml_tyxml.Tyxml_js.Html

type slide_change = Next | Prev

let slide_changer slide_change =
  let slide_div =
    let cls =
      match slide_change with
      | Next -> "tl-slidenav-next"
      | Prev -> "tl-slidenav-previous"
    in Manip.by_class cls
  in
  match slide_div with
  | [] -> Js_utils.log "Slide div has not been initialialized"; assert false
  | next :: _ -> next

let slide_reinit () =
  match Manip.by_class "tl-icon-goback" with
  | [] -> Js_utils.log "Reinit div has not been initialialized"; assert false
  | elt :: _ -> elt

let slide_event slide_change i = (* Clicks i times on next or prev *)
  let toclick = slide_changer slide_change in
  let rec loop i =
    if i <> 0 then begin
      Ui_utils.click toclick;
      loop (i - 1)
    end
  in loop i

let display_timeline title events =
  Js_utils.log "Displaying timeline";
  let title =
    match title with
    | None -> None
    | Some (_, t) -> Some t in
  let events =
    List.map (fun e ->
        let text =
          let t = e.text in
          let text = 
            match e.last_update with
            | None -> t.text
            | Some d -> 
              Format.asprintf
                "%s<br/><span class='last-date'>(Last updated: %a)</span>" 
                t.text
                (CalendarLib.Printer.Date.fprint "%F") d
          in
          {t with text} in
        {e with 
         group = None;
         text
        }
      ) events in
  let cmd =
    let timeline = {events; title} in
    let json = Json_encoding.construct (Data_encoding.timeline_encoding) timeline in
    let yoj  = Json_repr.to_yojson json in
    let str  = Yojson.Safe.to_string yoj in
    Format.asprintf
      "window.timeline = new TL.Timeline('timeline-embed',%s)"
      str in
  let () = Js_of_ocaml.Js.Unsafe.js_expr cmd in
  Js_utils.log "Timeline display done"

let url_position order =
  Js_utils.log "Url position";
  let path = Ui_utils.get_fragment () in
  match Utils.StringMap.find_opt path order with
  | None -> 0
  | Some (i, _) -> i

let go_to_right_slide order =
  Js_utils.log "Go to the right slide";
  let position = url_position order in
  Js_utils.log "Slide %i" position;
  slide_event Next position;
  Js_utils.log "Go to the right OK"

let url id = Ui_utils.url ("#" ^ id) []

let add_handlers_to_markers order =
  Js_utils.log "Add links to markers";
  let marker_order =
    let id_to_markerid i = i ^ "-marker" in  
    Utils.StringMap.fold
      (fun key bnd acc -> Utils.StringMap.add (id_to_markerid key) (key, bnd) acc)
      order
      Utils.StringMap.empty in
  let markers = Manip.by_class "tl-timemarker" in
  List.iter (
    fun elt ->
      let id = Ocp_js.Js.to_string @@ (Manip.get_elt "id" elt)##.id in
      Js_utils.log "Marker with id %s" id;
      match Utils.StringMap.find_opt id marker_order with
      | None ->
        Js_utils.log "Marker with id %s not found" id
      | Some (orig_id, (marker_pos, event)) ->
        let () = (* Adding handler *)
          let handler _ =
            let current_pos = url_position order in
            let diff = marker_pos - current_pos in
            let () =
              if diff < 0 then begin (* Go Prev *)
                slide_event Prev ((-1) * diff);
                let url = url orig_id in
                Ui_utils.push url
              end
              else if diff > 0 then begin
                slide_event Next diff;
                let url = url orig_id in
                Ui_utils.push url
              end
              else ()
            in Ocp_js.Js._true
          in
          Ocp_js.Dom.addEventListener
            (Manip.get_elt "click" elt)
            (Ocp_js.Dom.Event.make "click")
            (Ocp_js.Dom.handler handler)
            Ocp_js.Js._true |> ignore in
        let () = (* Adding image *)
          match event with
          | None -> ()
          | Some {media; _} -> begin
              match media with
              | None | Some {url = ""} -> ()
              | Some {url} ->
                let im = img ~a:[a_style "max-height: 11px"] ~src:url ~alt:"" () in
                match Manip.children elt with
                  _timespan :: content_container :: _ -> begin
                    match Manip.children content_container with
                      container :: _ -> begin
                        match Manip.children container with
                          media_container :: _ ->
                          Manip.replaceChildren media_container [im]
                        | _ -> ()
                      end
                    | _ -> ()
                  end
                | _ -> ()
            end
        in ()
  )
    markers;
  Js_utils.log "Markers OK"

let add_handlers_to_arrows order rev_order =
  Js_utils.log "Adding handlers to arrows";
  let push_next () =
    let current_pos = url_position order in
    match Utils.IntMap.find_opt (current_pos + 1) rev_order with
    | None ->
      Js_utils.log "Cannot find event at position %i" (current_pos + 1);
      Ocp_js.Js._false
    | Some path ->
      let url = url path in
      Ui_utils.push url;
      Ocp_js.Js._true
  in
  let push_prev () =
    let current_pos = url_position order in
    match Utils.IntMap.find_opt (current_pos - 1) rev_order with
    | None ->
      Js_utils.log "Cannot find event at position %i" (current_pos - 1);
      Ocp_js.Js._false
    | Some path ->
      let url = url path in
      Ui_utils.push url;
      Ocp_js.Js._true
  in
  let next = slide_changer Next in
  let prev = slide_changer Prev in
  let () = (* Next arrow link *)
    Ocp_js.Dom.addEventListener
      (Manip.get_elt "click" next)
      (Ocp_js.Dom.Event.make "click")
      (Ocp_js.Dom.handler (fun _k -> push_next ()))
      Ocp_js.Js._true |> ignore in

  let () = (* Prev arrow link *)
    Ocp_js.Dom.addEventListener
      (Manip.get_elt "click" prev)
      (Ocp_js.Dom.Event.make "click")
      (Ocp_js.Dom.handler (fun _ -> push_prev ()))
      Ocp_js.Js._true |> ignore in

  let () = (* Key press *)
    Ocp_js.Dom_html.(
      addEventListener
        document
        Event.keydown
        (handler (fun e ->
             match e##.keyCode with
             | 37 -> (* Left *)
               push_prev ()
             | 39 -> (* Right *)
               push_next ()
             | _ -> Ocp_js.Js._true
           ) 
        )
    )
      Ocp_js.Js._true |> ignore
  in 
  Js_utils.log "Handlers added to arrows"

let find_event unique_id events =
  List.find_opt
    (fun e -> unique_id = e.unique_id)
    events

let init_slide_from_url title events = begin
  let events =
    let e = List.map Utils.event_to_metaevent events in
    match title with
    | None -> e
    | Some (_, t) -> t :: e
  in
  Js_utils.log "Initializing timeline";
  (* Calculating in which order the slides are *)
  let _,order, rev_order =
    let slides = Manip.by_class "tl-slide" in (* In the page order *)
    List.fold_left (fun (cpt, acc_ord, acc_rev_ord) elt ->
          Js_utils.log "Reaching slide";
          let id = Ocp_js.Js.to_string @@ (Manip.get_elt "id" elt)##.id in
          Js_utils.log "Slide id: %s" id;
          let e = find_event id events in
          match e with
          | None ->
            Js_utils.log "Cannot find corresponding slide !";
            (cpt+1,
             Utils.StringMap.add id (cpt, None) acc_ord, Utils.IntMap.add cpt id acc_rev_ord)
          | Some e ->
            (cpt+1,
             Utils.StringMap.add id (cpt, Some e) acc_ord, Utils.IntMap.add cpt id acc_rev_ord)
      ) (0, Utils.StringMap.empty, Utils.IntMap.empty) slides
  in
  let () =
    Utils.IntMap.iter
      (fun i e -> Js_utils.log "%i --> %s" i e)
      rev_order in
  let () = go_to_right_slide       order in
  let () = add_handlers_to_markers order in
  let () = add_handlers_to_arrows  order rev_order in
  Js_utils.log "Timeline initialized";
  ()
end
