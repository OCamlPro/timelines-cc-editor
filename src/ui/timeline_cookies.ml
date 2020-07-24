open Json_encoding
open Ui_common

type t = {
  id : string;
  readonly : bool;
  name : string;
}

let t_encoding = 
  conv
    (fun {id; readonly; name} -> (id, readonly, name))
    (fun (id, readonly, name) -> {id; readonly; name})
    (obj3
      (req "url" string)
      (req "readonly" bool)
      (req "name" string))

let encoding = list t_encoding

let reset () =
  Cookie.set "timelines" "{}"

let get_timelines () =
  try
    List.iter
      (fun (k, _) -> Js_utils.log "Key %s" k) (Cookie.all ());
    let tls = List.assoc_opt "timelines" (Cookie.all ()) in
    match tls with
    | None -> []
    | Some str ->
      let json = Yojson.Safe.from_string str in
      let json = Json_repr.from_yojson json in
      destruct encoding json
  with _ ->
    let () = reset () in
    []

let add_timeline name id readonly =
  let tls = get_timelines () in
  let new_tl = {name; id; readonly} in
  let rec replace_or_add = function
    | [] -> [{id; readonly; name}]
    | old_timeline :: tl ->
      if old_timeline = new_tl then
        old_timeline :: tl
      else
        old_timeline :: replace_or_add tl in
  let new_tls = replace_or_add tls in
  let str = 
    Yojson.Safe.to_string @@ 
    Json_repr.to_yojson @@
    construct encoding new_tls in
  Cookie.set "timelines" str

let remove_timeline id =
  let tls = get_timelines () in
  let rec remove = function
    | [] -> []
    | ({id=old; _} as old_timeline) :: tl ->
      if old = id then
        remove tl
      else
        old_timeline :: remove tl in
  let new_tls = remove tls in
  let str = 
    Yojson.Safe.to_string @@ 
    Json_repr.to_yojson @@
    construct encoding new_tls in
  Cookie.set "timelines" str

let url t =
  let view = 
    if t.readonly then "view" else "edit" in
    Format.sprintf "%s?timeline=%s-%s"
      view
      t.name
      t.id

(* Js data for vuejs *)

class type urlData = object
  method name : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t Js_of_ocaml.Js.readonly_prop
  method url  : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t Js_of_ocaml.Js.readonly_prop
  method readonly : bool Js_of_ocaml.Js.readonly_prop
end

let js_data () =
  Js_of_ocaml.Js.array @@
  Array.of_list @@ 
  List.map
    (fun tl ->
       let obj : urlData Js_of_ocaml.Js.t =
         object%js
           val name = Ui_utils.jss tl.name
           val url = Ui_utils.jss @@ url tl
           val readonly = tl.readonly
         end in
       obj
    )
    (get_timelines ())
