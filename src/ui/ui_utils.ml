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
  (* remove when sessions are on *)
  a ~a:[a_href (link ~args path); a_class classes] content

type 'a input_type =
    Radio of string * string list
  | TextArea
  | Checkbox of bool (* true if checked *)
  | Number of int option * int option
  | Other of 'a

let placeholder
    ?(readonly = false)
    ?(classes=[])
    ?(content="")
    ?(input_type= Other `Text)
    ?title
    ?(style="")
    ?(newline=false)
    ~id
    ~name
    () : [> Html_types.div ] Js_of_ocaml_tyxml.Tyxml_js.Html.elt * (unit -> string option) =
  let to_form form =
    match title with
      None -> div form
    | Some elt ->
      if newline then
        div [
          (div ~a:[a_class [row]] [txt elt]);
          (div ~a:[a_class [row]] form)]
      else
        div ~a:[a_class [row]] [
          div ~a:[a_class [clg4]] [txt elt];
          div ~a:[a_class [clg8]] form]
  in
  let row ?(a = []) = div ~a:(a @ [a_class [row]]) in
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
                 let checked =
                   if String.(equal (lowercase_ascii str) value) then
                     [a_checked ()]
                   else [] in
                 let readonly =
                   if readonly then
                     [a_readonly ()]
                   else [] in
                 checked @ readonly @ a str
               in
               row [
                 input ~a ();
                 label [txt str];
               ]
            )
            l in
        let other =
          let hidden =
            if readonly then
              [a_style "visibility: hidden"]
            else [] in
          row ~a:hidden
            [input ~a:(a "__other__") ();
             input
               ~a:([
                   a_id "__other_value";
                   a_class (["placeholder"] @ classes);
                   a_style style;
                   a_input_type `Text;
                 ]) ()
            ]
        in to_form @@ [div ~a:[a_id id] (form_list @ [other])]
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
            a_class (["placeholder"] @ classes);
            a_value content;
            a_input_type `Checkbox;
            a_style (style ^ "margin-top:5%");
          ] in
          let default =
            if checked then
              a_checked () :: default
            else default
          in
          let default =
            if readonly then
              a_readonly () :: default
            else default
          in default
        in to_form [input ~a ()]
      in
      let getter () =
        try
          let elt = find_component id in
          let with_check = Js.Unsafe.coerce @@ Html.toelt elt in
          if with_check##.checked then begin
            Some "true"
          end else begin
            Some "false"
          end
        with
        | _ -> None in
      html_elt, getter
    end
  | TextArea -> begin
      let a =
        let default =
          [a_id id;
           a_class (["placeholder"] @ classes);
           a_style style;
          ] in
        if readonly then
          (a_readonly ()) :: default
        else default in
      let html_elt = to_form [textarea ~a (txt content)] in
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
        let readonly =
          if readonly then [a_readonly ()] else [] in
        min @ max @ readonly @ [
          a_id id;
          a_class (["placeholder"] @ classes);
          a_value content;
          a_input_type `Number;
          a_style style;
        ]
      in
      let html_elt = to_form [input ~a ()] in
      let getter =
        fun () ->
          match Manip.by_id id with
          | None -> Js_utils.log "Error: placeholder %s not found" id; None
          | Some e -> Some (Manip.value e) in
      html_elt, getter
    end
  | Other t -> begin
      let readonly =
        if readonly then [a_readonly ()] else [] in
      let html_elt =
        to_form @@ [
          input
            ~a:(readonly @ [
                a_id id;
                a_class (["placeholder"] @ classes);
                a_value content;
                a_input_type t;
                a_style style;
              ]) ()
        ]
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

module Session = struct
  let get_session () = Js.Optdef.to_option (Dom_html.window##.sessionStorage)

  let get_value key =
    let key' = Js.string key in
    match get_session () with
    | None ->
      Js_utils.log "Session not found while getting value";
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
    | None -> Js_utils.log "Session not found while setting value"
    | Some session ->
      let key   = Js.string key   in
      let value = Js.string value in
      session##setItem key value

  let remove_value key =
    let key' = Js.string key in
    match get_session () with
    | None ->
      Js_utils.log "Session not found while removing value"
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

let logout_session () =
  Session.remove_value "auth_data";
  Session.remove_value "email"

let hash s = string_of_int @@ Hashtbl.hash s

let download filename filecontent =
  Js_utils.log "Test:\n%s" filecontent;
  let filecontent = "data:text/csv;charset=us-ascii," ^ filecontent in
  let filelink =
    a ~a:[
      a_href "javascript:void(0);";
      a_download (Some filename);
      a_style "display: none";
    ][txt filecontent]
  in
  let body = Of_dom.of_body Dom_html.document##.body in
  Js_utils.Manip.appendChild body filelink;
  Manip.setAttribute filelink "href" filecontent;
  (Js_utils.Manip.get_elt "click" filelink)##click

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

let click elt = (Js_utils.Manip.get_elt "click" elt)##click

let slide_event slide_change i = (* Clicks i times on next or prev *)
  let toclick = slide_changer slide_change in
  let rec loop i =
    if i <> 0 then begin
      click toclick;
      loop (i - 1)
    end
  in loop i

let get_path () =
  match Jsloc.url () with
      Http h | Https h -> h.hu_path_string
    | File _ -> ""

let get_args () =
  match Jsloc.url () with
      Http h | Https h -> h.hu_arguments
    | File _ -> []

let assoc_add_unique key elt l =
  let rec loop acc = function
    | [] -> (key, elt) :: l
    | ((hd_key, hd_elt) as hd) :: tl ->
      if key = hd_key then
        (List.rev acc) @ ((hd_key, elt) :: tl)
      else loop (hd :: acc) tl
  in
  loop [] l

let assoc_add key elt l = (key, elt) :: l

let assoc_remove key l =
  let rec loop acc = function
    | [] -> List.rev acc
    | ((hd_key,_) as hd) :: tl ->
      if hd_key = key then
        loop acc tl
      else loop (hd :: acc) tl
  in loop [] l

let assoc_remove_with_binding key bnd l =
  let rec loop acc = function
    | [] -> List.rev acc
    | ((hd_key,hd_bnd) as hd) :: tl ->
      if hd_key = key && hd_bnd = bnd then
        loop acc tl
      else loop (hd :: acc) tl
  in loop [] l
  

let assoc_list key l =
  let rec loop acc = function
    | [] -> acc
    | (hd_key, hd_elt) :: tl ->
      if key = hd_key then
        loop (hd_elt :: acc) tl
      else loop acc tl
  in
  loop [] l

let clg = function
  | 1  -> clg1
  | 2  -> clg2
  | 3  -> clg3
  | 4  -> clg4
  | 5  -> clg5
  | 6  -> clg6
  | 7  -> clg7
  | 8  -> clg8
  | 9  -> clg9
  | 10 -> clg10
  | 11 -> clg11
  | 12 -> clg12
  | _ -> raise (Invalid_argument "clg")

let split_page id_current_page i =
  match Manip.by_id id_current_page with
  | None -> Js_utils.log "Page %s not found: aborting split" id_current_page
  | Some page ->
    let main_class = clg i in
    let main_id = id_current_page ^ "-1" in
    let split_class = clg (12 - i) in
    let split_id = id_current_page ^ "-2" in
    let children = Manip.children page in
    let new_child = div ~a:[a_class [row]] [
      div ~a:[a_class [main_class ]; a_id  main_id] children;
      div ~a:[a_class [split_class]; a_id split_id] [];
    ]
    in
    Manip.replaceChildren page [new_child]

let get_main_from_splitted id_splitted =
  Manip.by_id (id_splitted ^ "-1")

let get_split_from_splitted id_splitted =
  Manip.by_id (id_splitted ^ "-2")

let unsplit_page id_splitted =
  match Manip.by_id id_splitted with
  | None -> Js_utils.log "Page %s not found: aborting unsplit" id_splitted
  | Some page ->
    let children = Manip.children page in
    let row = Manip.children (List.hd children) in
    let first_page_children = Manip.children (List.hd row) in
    Manip.replaceChildren page first_page_children

let split_button
    id
    i
    split_button_text
    unsplit_button_text
    ~(action_at_split : unit -> bool)
    ~(action_at_unsplit : unit -> bool) =
  let split_button_id = id ^ "-split" in
  let unsplit_button_id = id ^ "-unsplit" in
  let split_button =
    div
      ~a:[
        a_class ["btn"; "btn-primary"];
        a_id split_button_id;
        a_onclick (fun _ ->
            split_page id i;
            show (find_component unsplit_button_id);
            hide (find_component split_button_id);
            action_at_split ())
      ] [txt split_button_text] in
  let unsplit_button =
    div
      ~a:[
        a_class ["btn"; "btn-primary"];
        a_id unsplit_button_id;
        a_onclick (fun _ ->
            unsplit_page id;
            hide (find_component unsplit_button_id);
            show (find_component split_button_id);
            action_at_unsplit ());
        a_style "display: none"
      ] [txt unsplit_button_text] in
  split_button, unsplit_button

let simple_button action t =
  div
    ~a:[a_class ["btn"; "btn-primary"];
        a_onclick (fun _ -> action (); true)]
    [txt t]

(* Returns a checkbox that performs action oncheck when the box is
   selected / unselected and a function that returns the current status
   of the box (Some true if selected, Some false if unselected, None if it does
   not exist in the page*)
let dynamic_checkbox
    ~classes
    ~checked
    ~style
    ~value
    ~id
    ~oncheck
    ~onuncheck =
  let ref_checked = ref checked in
  let onclick =
    fun _ ->
      if !ref_checked then begin
        ref_checked := false;
        onuncheck ()
      end else begin
        ref_checked := true;
        oncheck ()
      end; true
  in
  let html_elt =
    let a = [
      a_id id;
      a_class classes;
      a_value value;
      a_input_type `Checkbox;
      a_onclick onclick;
      a_style style
    ] in
    let a =
      if checked then
        a_checked () :: a
      else a
    in input ~a ()
  in
  let getter () = !ref_checked in
  html_elt, getter
