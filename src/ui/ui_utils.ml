open Js_of_ocaml_tyxml.Tyxml_js.Html

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
