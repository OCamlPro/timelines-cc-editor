open Js_of_ocaml.Url
open Lwt

let api () =
  Http {
    hu_host = Config.api_url;
    hu_port = Config.api_port;
    hu_path = [];
    hu_path_string = "";
    hu_arguments = [];
    hu_fragment = ""
  }

let events cont =
  let url = api () in
  let () = Js_utils.log "GET %s from %s" "events" (Js_of_ocaml.Url.string_of_url url) in
  Xhr_lwt.get ~base:url "events" >>=
  function
    Ok elt -> cont elt
  | Error e ->
    let code, msg = Xhr_lwt.error_content e in
    Js_utils.log "Error %i while getting to api: %s" code msg;
    Lwt.return (Error e)
