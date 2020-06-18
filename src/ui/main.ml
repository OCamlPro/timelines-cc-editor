open Js_utils
open Ocp_js

let () =
  Lang.init (fun () ->
    let path =
      match Jsloc.url () with
        Http h | Https h -> h.hu_path_string
      | File _ -> "file?" in
    ignore @@ Pages.dispatch ~path ~args:(Jsloc.args ())
 )
