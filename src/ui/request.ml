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
    Ok elt ->
    Js_utils.log "GET %s OK" apifun;
    cont elt
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
     Js_utils.log "Cooking";
     let yoj = Yojson.Safe.from_string str in
     let json = Json_repr.from_yojson yoj in
     let elt =
       try
         let elt = Json_encoding.destruct encoding json in
         Js_utils.log "Cooking OK";
         elt
       with e ->
         Js_utils.log "Error while cooking %s" str;
         raise e in
     cont elt
  )

let args_from_session args =
  match Ui_utils.get_auth_data () with
  | None -> args
  | Some (email, auth_data) ->
    ("auth_email", email) :: ("auth_data", auth_data) :: args

let timeline_data ~args cont =
  let args = args_from_session args in
  get ~args "timeline_data" (cook (Json_encoding.(list (tup2 int Data_encoding.event_encoding))) cont)

let raw_events ~args cont =
  let args = args_from_session args in
  get ~args "events" cont

let events ~args cont =
  raw_events ~args (cook (Json_encoding.(list (tup2 int Data_encoding.event_encoding))) cont)

let title ~args cont =
  get ~args "title" (cook (Json_encoding.(tup1 @@ option (Data_encoding.title_encoding))) cont)

let event ~args id cont =
  let args = args_from_session args in
  get ~args (Format.sprintf "event/%i" id)  (cook Data_encoding.event_encoding cont)

let add_event ~args (event : Data_types.event) cont =
  let args = args_from_session args in
  post ~args
    "add_event"
    Data_encoding.event_encoding event
    ApiData.api_result_encoding cont

let update_event ~args id ~old_event ~new_event cont =
  let args = args_from_session args in
  post ~args
    "update_event"
    Json_encoding.(
      tup3
        (tup1 int)
        Data_encoding.event_encoding Data_encoding.event_encoding)
    (id, old_event, new_event)
    ApiData.update_event_res_encoding cont

let update_title ~args ~old_title ~new_title cont =
  let args = args_from_session args in
  post ~args
    "update_title"
    (Json_encoding.tup2
       Data_encoding.title_encoding
       Data_encoding.title_encoding)
    (old_title, new_title)
    ApiData.update_title_res_encoding cont

let categories cont = get "categories" (cook (Json_encoding.(list string)) cont)

let remove_event ~args id cont =
  let args = args_from_session args in
  get ~args (Format.sprintf "remove_event/%i" id) cont

let register_user email password cont =
  Js_utils.log "Request register_user@.";
  let hash = Ui_utils.hash password (* todo: change this *) in
  Js_utils.log "Hash: %s@." hash;
  post ~args:[] "register_user"
    Json_encoding.(tup2 string string) (email, hash)
    ApiData.api_result_encoding cont

let login email password cont =
  let hash = Ui_utils.hash password (* todo: change this *) in
  Js_utils.log "Hash: %s@." hash;
  post ~args:[] "login"
    Json_encoding.(tup2 string string) (email, hash)
    Json_encoding.(tup1 @@ option string) cont

let is_auth cont =
  post ~args:(args_from_session []) "is_auth"
    Json_encoding.unit ()
    Json_encoding.(tup1 bool) cont

let logout cont =
  match Ui_utils.get_auth_data () with
  | None -> Lwt.return @@ Error (Xhr_lwt.Str_err "Error: not logged in")
  | Some (email, auth_data) ->
    post ~args:[] "logout"
      (Json_encoding.(tup2 string string)) (email, auth_data)
      (Json_encoding.unit) cont
