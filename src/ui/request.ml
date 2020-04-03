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

let get ?args apifun cont =
  let url = api () in
  let () = Js_utils.log "GET %s from %s" apifun (Js_of_ocaml.Url.string_of_url url) in
  Xhr_lwt.get ?args ~base:url apifun >>=
  function
    Ok elt -> cont elt
  | Error e ->
    let code, msg = Xhr_lwt.error_content e in
    Js_utils.log "Error %i while getting to api: %s" code msg;
    Lwt.return (Error e)

let post apifun input cont =
  let () = Js_utils.log "POST %s" apifun in
  let url = api () in
  let () = Js_utils.log "Calling API at %s" (Js_of_ocaml.Url.string_of_url url) in
  Xhr_lwt.post ~base:url Data_encoding.event_encoding Json_encoding.bool apifun input >>=
  function
    Ok elt -> cont elt
  | Error e ->
    let code, msg = Xhr_lwt.error_content e in
    Js_utils.log "Error %i while getting to api: %s" code msg;
    Lwt.return (Error e)

let cook encoding cont =
  (fun str ->
     let yoj = Yojson.Safe.from_string str in
     let json = Json_repr.from_yojson yoj in
     let elt = Json_encoding.destruct encoding json in
     cont elt)

let raw_events cont = get "events" cont
let events cont = raw_events (cook (Json_encoding.list Data_encoding.event_encoding) cont)

let event id cont = get "event" ~args:id cont

let add_event event cont = post "add_event" event cont