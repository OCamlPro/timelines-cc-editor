(* open Js_of_ocaml_tyxml.Tyxml_js.Html *)
(* open Ocp_js *)
(* module Dom_html = Js_of_ocaml_tyxml.Tyxml_js.Html *)
module Dom_html = Js_of_ocaml.Dom_html
module Html = Ezjs_tyxml.Html
module Js = Js_of_ocaml.Js
module Loc = Ezjs_loc
module Manip = Ezjs_tyxml.Manip
module Url = Js_of_ocaml.Url

module Session = struct
  let get_session () = Js.Optdef.to_option (Dom_html.window##.sessionStorage)

  let get_value key =
    let key' = Js.string key in
    match get_session () with
    | None ->
      Ezjs_tyxml.log "Session not found while getting value";
      None
    | Some session -> begin
        match Js.Opt.to_option (session##getItem key') with
        | None -> None
        | Some s ->
          let result = Js.to_string s in
          Some result
      end

  let set_value key value =
    match get_session () with
    | None -> Ezjs_tyxml.log "Session not found while setting value"
    | Some session ->
      let key   = Js.string key   in
      let value = Js.string value in
      session##setItem key value

  let remove_value key =
    let key' = Js.string key in
    match get_session () with
    | None ->
      Ezjs_tyxml.log "Session not found while removing value"
    | Some session -> begin
        match Js.Opt.to_option (session##getItem key') with
        | None -> ()
        | Some _ -> session##removeItem key'
      end
end

let auth_session email auth_data =
  Session.set_value "email" email;
  Session.set_value "auth_data" auth_data

let get_auth_data () =
  match Session.get_value "email", Session.get_value "auth_data" with
    Some e, Some a -> Some (e, a)
  | _ -> None

let hash _s = failwith "Unimplemented hash function Ui_utils.hash"

let trim s = String.map (function ' ' -> '-' | c -> c) s

let jss = Js.string

let goto_page s = Dom_html.window##.location##.href := jss s

let click elt = (Manip.get_elt "click" elt)##click

let link ?(args=[]) path =
  match args with
  | [] -> path
  | _ ->
    let args =
      String.concat "&"
        (List.map (fun (key, value) -> key ^ "=" ^ value) args)
    in
    if String.contains path '?' then
      Printf.sprintf "%s&%s" path args
    else
      Printf.sprintf "%s?%s" path args


let a_link ?(args=[]) ?(classes=[]) ~path content =
  (* remove when sessions are on *)
  Html.a ~a:[Html.a_href (link ~args path); Html.a_class classes] content

let get_fragment () =
  match Loc.url () with
  | Http h | Https h -> h.hu_fragment
  | File f -> f.fu_fragment

let url path args =
  let rec loop acc = function
      [] -> acc
    | (k,v) :: tl ->
      let acc = Format.sprintf "%s&%s=%s" acc k v in
      loop acc tl
  in
  match args with
    [] -> path
  | (k, v) :: tl ->
    let start = Format.sprintf "%s?%s=%s" path k v in
    loop start tl

let push url =
    let url' = Js.string url in
    Dom_html.window##.history##pushState Js.null url' (Js.some url')

let replace url =
    let url' = Js.string url in
    Dom_html.window##.history##replaceState Js.null url' (Js.some url')

let download filename filecontent =
  Ezjs_tyxml.log "Test:\n%s" filecontent;
  let filecontent = "data:text/csv;charset=utf-8," ^ Url.urlencode filecontent in
  let filelink =
    Html.a ~a:[
      Html.a_href filecontent;
      Html.a_download (Some filename);
      Html.a_style "display: none";
    ][Html.txt filecontent]
  in
  let body = Ezjs_tyxml.Of_dom.of_body Dom_html.document##.body in
  Manip.appendChild body filelink;
  (Manip.get_elt "click" filelink)##click

let get_path () =
  match Loc.url () with
    Http h | Https h -> h.hu_path_string
  | File _ -> ""

let get_host () =
  match Loc.url () with
    Http h | Https h -> h.hu_host
  | File _ -> ""

let is_localhost () =
  get_host () = "localhost"

let get_port () =
  match Loc.url () with
    Http h | Https h -> h.hu_port
  | File _ -> 80

let is_https () =
  match Loc.url () with
    Http _ | File _ -> false
  | Https _ -> true

let get_url_prefix () =
  if is_https () then "https://" else "http://"

let timeline_arg_from_id ?name id =
  match name with
    None -> id
  | Some name -> name ^ "-" ^ id

let timeline_id_from_arg a =
  match List.rev (String.split_on_char '-' a) with
  | [] -> assert false
  | [id] -> "Timeline", id
  | id :: rest ->
    String.concat " " (List.rev rest),
    id

let list_to_jsarray l =
  Js.array @@ Array.of_list l

let slow_hide elt =
  let open Lwt in
  let opacity =
    match Manip.Css.opacity elt with
    | Some s -> begin try float_of_string s with _ -> 1. end
    | None  -> 1. in
    let rec loop opacity =
      if opacity <= 0. then begin
        Ezjs_tyxml.hide elt;
        Lwt.return ()
      end else
        let new_opacity = opacity -. 0.1 in
        Manip.SetCss.opacity elt @@ Some (string_of_float new_opacity);
        Js_of_ocaml_lwt.Lwt_js.sleep 0.03 >>= (fun () ->
            loop new_opacity) in
  ignore @@ loop opacity

let update_page_title title =
  Dom_html.document##.title := Js.string (title ^ " - Timelines.cc")

let unURLize str =
  let rx = Js_of_ocaml.Regexp.regexp_string "%20" in
  Js_of_ocaml.Regexp.global_replace rx " " str
