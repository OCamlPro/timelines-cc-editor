open Js_utils
open Data_types
open Js_of_ocaml_tyxml.Tyxml_js.Html

let default_ponderation = 10

let allowing_dynamic_slides = [
  "timeline.ocamlpro.com";
  "timeline-standalone.k.ocaml.pro";
  "localhost"
]

let allow_init () =
  let hostname = Ui_utils.get_hostname () in
  Js_utils.log "Anchor = %s" hostname;
  List.mem hostname allowing_dynamic_slides

let display_timeline () =
  Request.timeline_data ~args:["ponderation", string_of_int default_ponderation] (fun events ->
      Js_utils.log "Timeline Data OK";
      Request.title ~args:[] (fun title ->
          Js_utils.log "Title OK";
          let events =
            List.map (fun (_,e) ->
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
          Lwt.return (Ok {title; events})
        )
    )

let url_position has_title order =
  Js_utils.log "Url position";
  let path = Ui_utils.get_fragment () in
  match Utils.StringMap.find_opt path order with
  | None -> 0
  | Some (i, _) -> if has_title then i+1 else i

let go_to_right_slide has_title order =
  Js_utils.log "Go to the right slide";
  let position = url_position has_title order in
  Js_utils.log "Slide %i" position;
  Ui_utils.slide_event Next position

let url id = Ui_utils.url ("#" ^ id) []

let add_handlers_to_markers has_title order =
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
            let current_pos = url_position has_title order in
            let diff = marker_pos - current_pos in
            let () =
              if diff < 0 then begin (* Go Prev *)
                Ui_utils.slide_event Prev ((-1) * diff);
                let url = url orig_id in
                Ui_utils.push url
              end
              else if diff > 0 then begin
                Ui_utils.slide_event Next diff;
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
          match event.media with
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
        in ()
  )
    markers

let add_handlers_to_arrows has_title order rev_order =
  let push_next () =
    let current_pos = url_position has_title order in
    match Utils.IntMap.find_opt (current_pos + 1) rev_order with
    | None ->
      Js_utils.log "Cannot find event at position %i" (current_pos - 1);
      Ocp_js.Js._false
    | Some path ->
      let url = url path in
      Ui_utils.push url;
      Ocp_js.Js._true
  in
  let push_prev () =
    let current_pos = url_position has_title order in
    match Utils.IntMap.find_opt (current_pos - 1) rev_order with
    | None ->
      Js_utils.log "Cannot find event at position %i" (current_pos - 1);
      Ocp_js.Js._false
    | Some path ->
      let url = url path in
      Ui_utils.push url;
      Ocp_js.Js._true
  in
  let next = Ui_utils.slide_changer Next in
  let prev = Ui_utils.slide_changer Prev in
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
  in ()

let find_event unique_id events =
  List.find
    (fun e -> unique_id = e.unique_id)
    events

let init_slide_from_url res =
  if not (allow_init ()) then
    Lwt.return (Ok ())
  else begin 
    match res with
    | Error e ->
      Js_utils.log "Error before initializing slide";
      Lwt.return (Error e)
    | Ok {title; events} -> begin
        let has_title =
          match title with
          | None -> false
          | Some _ -> true in
        (* Calculating in which order the slides are *)
        let _,order, rev_order =
          let slides = Manip.by_class "tl-slide" in (* In the page order *)
          List.fold_left (fun (cpt, acc_ord, acc_rev_ord) elt ->
              try
                let id = Ocp_js.Js.to_string @@ (Manip.get_elt "id" elt)##.id in
                let e = find_event id events in
                (cpt+1, Utils.StringMap.add id (cpt, e) acc_ord, Utils.IntMap.add cpt id acc_rev_ord)
              with _ -> (cpt+1, acc_ord, acc_rev_ord)
            ) (0, Utils.StringMap.empty, Utils.IntMap.empty) slides
        in
        let () = go_to_right_slide       has_title order in
        let () = add_handlers_to_markers has_title order in
        let () = add_handlers_to_arrows  has_title order rev_order in
        Lwt.return (Ok ())
      end
  end
                
let () = let open Lwt in
  (display_timeline () >>= init_slide_from_url) >>= (fun _ -> Lwt.return (Ok ())) |> ignore
