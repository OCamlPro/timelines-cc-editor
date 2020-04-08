open Js_of_ocaml_tyxml.Tyxml_js.Html
open Bootstrap_helpers
open Grid
open Form
open Js_utils
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Ocp_js

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
  a ~a:[a_href (link ~args path); a_class classes] content

type 'a input_type =
    Radio of string * string list
  | TextArea
  | Checkbox of bool (* true if checked *)
  | Number of int option * int option
  | Other of 'a

let placeholder
    ?(classes=[])
    ?(content="")
    ?(input_type= Other `Text)
    ?title
    ?(style="")
    ~id
    ~name
    () : [> Html_types.div ] Js_of_ocaml_tyxml.Tyxml_js.Html.elt * (unit -> string option) =
  let to_form form =
    match title with
      None -> form
    | Some elt -> div ~a:[a_class [clg3]] [txt elt] :: form
  in
  let row = div ~a:[a_class [row]]
  in
  match input_type with
  | Radio (value, l) -> begin
      let html_elt =
        let value = String.lowercase_ascii value in
        let a str = [
          a_class (["placeholder"; form_inline] @ classes);
          a_value str;
          a_input_type `Radio;
          a_name name;
          a_style style
        ] in
        let form_list =
          List.map
            (fun str ->
               let a =
                 let default = a str in
                 if String.(equal (lowercase_ascii str) value) then
                   a_checked () :: default
                 else default in
               row [
                 input ~a ();
                 label [txt str];
               ]
            )
            l in
        let other =
          row [input ~a:(a "__other__") ();
           input
             ~a:[
               a_id "__other_value";
               a_class (["placeholder"] @ classes);
               a_style style;
               a_input_type `Text
             ] ()
          ]
        in
        row (to_form @@ [div ~a:[a_class [clg9]; a_id id] (form_list @ [other])])
      in
      let getter () =
        let form = Js_utils.find_component id in
        let input_list = Js_utils.Manip.children form in
        let rec loop = function
          | [] -> Js_utils.log "Error: no checked checkbox"; None
          | hd :: tl -> begin
              let children = Js_utils.Manip.children hd in
              match
                List.find_opt (
                  fun child ->
                    let with_check = Js.Unsafe.coerce @@ Html.toelt child in
                    with_check##.checked
                )
                  children
              with
              | None -> loop tl
              | Some elt -> begin
                  let value = Manip.value elt in
                  if value = "__other__" then
                    Some (Manip.value (find_component "__other_value"))
                  else Some (Manip.value elt)
                end
            end
        in loop input_list
      in
      html_elt, getter
    end
  | Checkbox checked -> begin
      let html_elt =
        let a =
          let default =  [
            a_id id;
            a_class (["placeholder"; clg9] @ classes);
            a_value content;
            a_input_type `Checkbox;
            a_style style;
          ] in
          if checked then
            a_checked () :: default
          else default in
        row (
          to_form [input ~a ()]
        )
      in
      let getter () =
        try
          let elt = find_component id in
          let with_check = Js.Unsafe.coerce @@ Html.toelt elt in
          if with_check##.checked then
            Some "true"
          else Some "false"
        with
        | _ -> None in
      html_elt, getter
    end
  | TextArea -> begin
      let html_elt =
        row (
          to_form @@ [
            textarea
              ~a:[a_id id;
                  a_class (["placeholder"; clg9] @ classes);
                  a_style style;
                 ] (txt content)
          ]
        ) in
      let getter =
        fun () ->
          match Manip.by_id id with
          | None -> Js_utils.log "Error: placeholder %s not found" id; None
          | Some e -> Some (Manip.value e) in
      html_elt, getter
    end
  | Number (min, max) -> begin
      let a =
        let min =
          match min with
          | None -> []
          | Some i -> [a_input_min @@ `Number i] in
        let max =
          match max with
          | None -> []
          | Some i -> [a_input_max @@ `Number i] in
        min @ max @ [
          a_id id;
          a_class (["placeholder"; clg9] @ classes);
          a_value content;
          a_input_type `Number;
          a_style style;
        ]
      in
      let html_elt = row (to_form [input ~a ()]) in
      let getter =
        fun () ->
          match Manip.by_id id with
          | None -> Js_utils.log "Error: placeholder %s not found" id; None
          | Some e -> Some (Manip.value e) in
      html_elt, getter
    end
  | Other t -> begin
      let html_elt =
        row (
          to_form @@ [
            input
              ~a:[a_id id;
                  a_class (["placeholder"; clg9] @ classes);
                  a_value content;
                  a_input_type t;
                  a_style style;
                 ] ()
          ]
        )
      in
      let getter =
        fun () ->
          match Manip.by_id id with
          | None -> Js_utils.log "Error: placeholder %s not found" id; None
          | Some e -> Some (Manip.value e) in
      html_elt, getter
    end


let add_text_to_placeholder id t =
  match Manip.by_id id with
    None -> failwith "better error mesg"
  | Some s -> Manip.replaceChildren s [txt t]

let get_value id =
  match Manip.by_id id with
    None -> ""
  | Some elt -> Manip.value elt

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

let make_id prefix suffix = Printf.sprintf "%s-%s" prefix suffix

let make_loading_gif classes =
  div ~a:[ a_class classes ] [
    img
      ~alt:"loading"
      ~src:(uri_of_string "/images/loading.gif") ()
  ]

let span_loading_gif classes =
  span ~a:[ a_class classes ] [
    img
      ~alt:"loading"
      ~src:(uri_of_string "/images/loading.gif") ()
  ]

(* Pagination utilities *)

let set_url_arg ?(default = "1") arg value =
  let args = Jsloc.args () in
  let replaced = ref false in
  let args = List.fold_right (fun (k, v) newargs ->
                 if k = arg then begin
                     replaced := true;
                     if value = default then
                       newargs
                     else
                       (k, value) :: newargs
                   end
                 else (k, v) :: newargs
               ) args [] in
  let args = if !replaced || value = default then args else
      (arg, value) :: args in
  Jsloc.set_args args

let select_page_from_list page page_size l =
  let rec remove i l =
    match i, l with
    | 0, _ | _, [] -> l
    | _, _hd :: tl -> remove (i - 1) tl in

  let rec select acc i l =
    match i, l with
    | 0, _ | _, [] -> List.rev acc
    | _, hd :: tl  -> select (hd :: acc) (i - 1) tl in

  let rec go_to_page page l =
    if page = 0 then l
    else go_to_page (page - 1) (remove page_size l)
  in

  select [] page_size (go_to_page page l)
