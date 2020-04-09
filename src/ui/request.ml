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

let get ?(args = []) apifun cont =
  let url = api () in
  let () =
    Js_utils.log "GET %s from %s with args [%a]"
      apifun
      (Js_of_ocaml.Url.string_of_url url)
      (Format.pp_print_list
         ~pp_sep:(fun fmt _ -> Format.fprintf fmt "; ")
         (fun fmt (arg, value) -> Format.fprintf fmt "%s = %s" arg value)) args
  in
  Xhr_lwt.get ~args ~base:url apifun >>=
  function
    Ok elt -> cont elt
  | Error e ->
    let code, msg = Xhr_lwt.error_content e in
    Js_utils.log "Error %i while getting to api: %s" code msg;
    Lwt.return (Error e)

let post ~args apifun input_encoding input output_encoding cont =
  let () = Js_utils.log "POST %s" apifun in
  let url = api () in
  let () = Js_utils.log "Calling API at %s" (Js_of_ocaml.Url.string_of_url url) in
  Xhr_lwt.post ~args ~base:url input_encoding output_encoding apifun input >>=
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

let timeline_data ~args cont =
  get ~args "timeline_data" (cook (Json_encoding.(list (tup2 int Data_encoding.event_encoding))) cont)

let events ~args cont =
  get ~args "events" (cook (Json_encoding.(list (tup2 int Data_encoding.event_encoding))) cont)

let event ~args id cont =
  get ~args (Format.sprintf "event/%i" id)  (cook Data_encoding.event_encoding cont)

let add_event ~args (event : Data_types.event) cont =
  post ~args
    "add_event"
    Data_encoding.event_encoding event
    Data_encoding.api_result_encoding cont

let update_event ~args id event cont =
  post ~args
    "update_event"
    Json_encoding.(tup2 (tup1 int) Data_encoding.event_encoding) (id, event)
    Data_encoding.api_result_encoding cont

let categories cont = get "categories" (cook (Json_encoding.(list string)) cont)

let remove_event ~args id cont =
  get ~args (Format.sprintf "remove_event/%i" id) cont
