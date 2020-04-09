open Js_utils
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Bootstrap_helpers.Grid
open Ocp_js

let () =
  let path_str, args =
    match Jsloc.url () with
      Http h | Https h -> h.hu_path_string, h.hu_arguments
    | File _ -> "file?", [] in
  ignore @@ Pages.dispatch path_str args
