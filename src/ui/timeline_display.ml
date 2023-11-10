(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

open Timeline_data
open Data_types
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Ui_common

module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Manip = Ezjs_tyxml.Manip
module Js = Js_of_ocaml.Js
module Timeline = Ezjs_timeline.Timeline

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
  | [] -> Ezjs_tyxml.log "Slide div has not been initialialized"; assert false
  | next :: _ -> next

let slide_reinit () =
  match Manip.by_class "tl-icton-goback" with
  | [] -> Ezjs_tyxml.log "Reinit div has not been initialialized"; assert false
  | elt :: _ -> elt

let slide_event slide_change i = (* Clicks i times on next or prev *)
  let toclick = slide_changer slide_change in
  let rec loop i =
    if i <> 0 then begin
      Ui_utils.click toclick;
      loop (i - 1)
    end
  in loop i

let display_timeline ?(view=false) title events =
  Ezjs_tyxml.log "[display_timeline] Displaying timeline";
  let title =
    match title with
    | None -> None
    | Some (_, t) -> Some t in
  let events =
    List.map (fun (_, e) ->
        let text =
          let t = e.text in
          let text =
            if view then
              t.text
            else
              match e.last_update with
              | None -> t.text
              | Some d -> 
                Format.asprintf
                  "%s<br/><span class='last-date'>(Last updated: %a)</span>" 
                  t.text
                  (CalendarLib.Printer.Date.fprint "%F") d
          in
          {t with text} in
        {e with text}
      ) events in
  let json =
    let timeline = {events; title} in
    let json = Json_encoding.construct (Data_encoding.timeline_encoding) timeline in
      Format.asprintf "%a" (Json_repr.pp ~compact:true (module Json_repr.Ezjsonm)) json
  in
  let () = Timeline.make "timeline-embed" (SStr json) in
  Ezjs_tyxml.log "Timeline display done"

let url_position order (rev_order : string Utils.IntMap.t) =
  Ezjs_tyxml.log "Url position";
  let path = Ui_utils.get_fragment () in
  Ezjs_tyxml.log "Url position: %s" path;
  match Utils.StringMap.find_opt path order with
  | None -> begin
    Ezjs_tyxml.log "Path %s has not been found, assuming first slide" path;
    match Utils.IntMap.find_opt 0 rev_order with
    | None -> Ezjs_tyxml.log "Timeline has no event"; failwith "Timeline has no event"
    | Some i -> 0, Some i
  end
  | Some (i, None) -> i, None
  | Some (i, Some e) -> i, Some (e.unique_id)

let go_to_right_slide ~(whenOnSlide : string option -> unit) order rev_order =
  Ezjs_tyxml.log "Go to the right slide";
  let position, id = url_position order rev_order in
  Ezjs_tyxml.log "Slide %i" position;
  slide_event Next position;
  whenOnSlide id;
  Ezjs_tyxml.log "Go to the right OK"

let url id = Ui_utils.url ("#" ^ id) []

let add_handlers_to_markers ~(whenOnSlide:string option -> unit) order rev_order =
  Ezjs_tyxml.log "Add links to markers";
  let marker_order =
    let id_to_markerid i = i ^ "-marker" in
    Utils.StringMap.fold
      (fun key bnd acc -> Utils.StringMap.add (id_to_markerid key) (key, bnd) acc)
      order
      Utils.StringMap.empty in
  let markers = Manip.by_class "tl-timemarker" in
  List.iter (
    fun elt ->
      let id = Js.to_string @@ (Manip.get_elt "id" elt)##.id in
      match Utils.StringMap.find_opt id marker_order with
      | None ->
        Ezjs_tyxml.log "Marker with id %s not found" id
      | Some (orig_id, (marker_pos, event)) ->
        let () = (* Adding handler *)
          let handler _ =
            let current_pos,_ = url_position order rev_order in
            let diff = marker_pos - current_pos in
            let () =
              if diff < 0 then begin (* Go Prev *)
                slide_event Prev ((-1) * diff);
                let url = url orig_id in
                Ui_utils.replace url;
              end
              else if diff > 0 then begin
                slide_event Next diff;
                let url = url orig_id in
                Ui_utils.replace url;
              end
              else ()
            in
            whenOnSlide (Some orig_id);
            Js._true
          in
          Dom.addEventListener
            (Manip.get_elt "click" elt)
            (Dom.Event.make "click")
            (Dom.handler handler)
            Js._true |> ignore in
        let () = (* Adding images *)
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
  Ezjs_tyxml.log "Markers OK"

let add_handlers_to_arrows
    ~(whenOnSlide : string option -> unit)
    ~(activate_keypress : unit -> bool)
    order
    rev_order =
  Ezjs_tyxml.log "Adding handlers to arrows";
  let push_next () =
      Ezjs_tyxml.log "Push next";
      let current_pos,_ = url_position order rev_order in
      match Utils.IntMap.find_opt (current_pos + 1) rev_order with
      | None ->
        Ezjs_tyxml.log "Cannot find event at position %i" (current_pos + 1);
        whenOnSlide None;
        Js._false
      | Some path ->
        let url = url path in
        Ui_utils.replace url;
        whenOnSlide (Some path);
        Js._true
  in
  let push_prev () =
      Ezjs_tyxml.log "Push prev";
      let current_pos,_ = url_position order rev_order in
      match Utils.IntMap.find_opt (current_pos - 1) rev_order with
      | None ->
        Ezjs_tyxml.log "Cannot find event at position %i" (current_pos - 1);
        whenOnSlide None;
        Js._false
      | Some path ->
        let url = url path in
        Ui_utils.replace url;
        whenOnSlide (Some path);
        Js._true
  in
  let next = slide_changer Next in
  let prev = slide_changer Prev in
  let () = (* Next arrow link *)
    Dom.addEventListener
      (Manip.get_elt "click" next)
      (Dom.Event.make "click")
      (Dom.handler (fun _k -> push_next ()))
      Js._true |> ignore in

  let () = (* Prev arrow link *)
    Dom.addEventListener
      (Manip.get_elt "click" prev)
      (Dom.Event.make "click")
      (Dom.handler (fun _ -> push_prev ()))
      Js._true |> ignore in

  let () = (* Key press *)
    Dom_html.(
      addEventListener
        document
        Event.keydown
        (handler (fun e ->
             if activate_keypress () then begin
               Ezjs_tyxml.log "Acivate Keypress = true";
               match e##.keyCode with
               | 37 -> (* Left *)
                 push_prev ()
               | 39 -> (* Right *)
                 push_next ()
               | _ -> Js._true
             end else begin
               Ezjs_tyxml.log "Acivate Keypress = false";
               Js._true
             end
           )
        )
    )
      Js._true |> ignore
  in
  Ezjs_tyxml.log "Handlers added to arrows"

let find_event unique_id events =
  List.find_opt
    (fun (_, e) -> unique_id = e.unique_id)
    events

(* Small hack for fixing centering issue *)
let resize_tl_slide_content () =
  List.iter
    (fun elt -> Manip.SetCss.width elt "100%")
    (Manip.by_class "tl-slide-content")

let init_slide_from_url ~whenOnSlide ~activate_keypress title events = begin
  let events =
    let e = List.map (fun (i, e) -> i, (Utils.event_to_metaevent e)) events in
    match title with
    | None -> e
    | Some t -> t :: e
  in
  Ezjs_tyxml.log "Initializing timeline";
  (* Calculating in which order the slides are *)
  let _,order, rev_order =
    let slides = Manip.by_class "tl-slide" in (* In the page order *)
    List.fold_left (fun (cpt, acc_ord, acc_rev_ord) elt ->
      let id = Js.to_string @@ (Manip.get_elt "id" elt)##.id in
      let e = find_event id events in
      match e with
      | None ->
        Ezjs_tyxml.log "Cannot find corresponding slide !";
        (cpt+1,
         Utils.StringMap.add id (cpt, None) acc_ord,
         Utils.IntMap.add cpt id acc_rev_ord)
      | Some (_, e) -> (* db_id is the integer ID of the event *)
        (cpt+1,
         Utils.StringMap.add id (cpt, Some e) acc_ord,
         Utils.IntMap.add cpt id acc_rev_ord)
      ) (0, Utils.StringMap.empty, Utils.IntMap.empty) slides
  in
  let () = go_to_right_slide       ~whenOnSlide order rev_order in
  let () = add_handlers_to_markers ~whenOnSlide order rev_order in
  let () =
    add_handlers_to_arrows
      ~whenOnSlide
      ~activate_keypress
      order
      rev_order in
  let () = resize_tl_slide_content () in
  Ezjs_tyxml.log "Timeline initialized";
  ()
end
