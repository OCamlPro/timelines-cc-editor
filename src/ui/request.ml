open Js_of_ocaml.Url
open Lwt

let api () =
  Http {
    hu_host = "d4.dune.network";
    hu_port = Config.api_port;
    hu_path = [];
    hu_path_string = "d4.dune.network:23456";
    hu_arguments = [];
    hu_fragment = ""    
  }
(*
  match Js_of_ocaml.Url.Current.get () with
  | Some u -> u
  | None ->
    match Js_of_ocaml.Url.url_of_string "http://localhost:8080" with
    | Some u -> u
    | None -> assert false *)

let get ?(args = []) apifun cont =
  let url = api () in (*
  let url = (* Only for standalone !! *)
    match url wit<h
    | Http u -> Http {u with hu_path_string = ""}
    | Https u -> Https {u with hu_path_string = ""}
    | File f -> File {f with fu_path_string = ""} in *) 
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
    Js_utils.log "GET %s OK: %s" apifun elt;
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
       with
       | e ->
         Js_utils.log "Error while cooking %s: %s"
           str
           (Printexc.to_string e)
         ;
         raise e in
     cont elt
  )

let args_from_session args =
  match Ui_utils.get_auth_data () with
  | None -> args
  | Some (email, auth_data) ->
    ("auth_email", email) :: ("auth_data", auth_data) :: args

let timeline_data ~args timeline cont =
  let args = args_from_session args in
  get
    ~args
    (Format.sprintf "timeline_data/%s" timeline)
    (cook (Json_encoding.(list (tup2 int Data_encoding.event_encoding))) cont)

let event ~args (id : int) cont =
  let args = args_from_session args in
  get ~args (Format.sprintf "event/%i" id)  (cook Data_encoding.title_encoding cont)

let events ~args (tid : string) cont =
  let args = args_from_session args in
  get
    ~args
    (Format.sprintf "events/%s" tid)
    (cook (Json_encoding.(list (tup2 int Data_encoding.event_encoding))) cont)

let title ~args tid cont =
  get
    ~args
    (Format.sprintf "title/%s" tid)
    (cook (Json_encoding.(tup1 @@ option (Data_encoding.title_encoding))) cont)

let add_event ~args (tid : string) (event : Data_types.event) cont =
  let args = args_from_session args in
  post ~args
    (Format.sprintf "add_event/%s" tid)
    Data_encoding.event_encoding event
    ApiData.api_result_encoding cont

let update_event id ~old_event ~new_event cont =
  let args = args_from_session ["id", string_of_int id] in
  post ~args
    "update_event"
    Json_encoding.(
      tup3
        (tup1 int)
        Data_encoding.title_encoding Data_encoding.title_encoding)
    (id, old_event, new_event)
    ApiData.update_event_res_encoding cont

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

let has_admin_rights timeline cont =
  post
    ~args:(args_from_session [])
    (Format.sprintf "has_admin_rights/%s" timeline)
    Json_encoding.unit ()
    Json_encoding.(tup1 bool) cont

let categories timeline cont =
  get
    ~args:(args_from_session [])
    (Format.sprintf "categories/%s" timeline)
    (cook (Json_encoding.(list string)) cont)

let logout cont =
  match Ui_utils.get_auth_data () with
  | None -> Lwt.return @@ Error (Xhr_lwt.Str_err "Error: not logged in")
  | Some (email, auth_data) ->
    post ~args:[] "logout"
      (Json_encoding.(tup2 string string)) (email, auth_data)
      (Json_encoding.unit) cont

let create_timeline title cont =
  post 
    ~args:(args_from_session [])
    (Format.sprintf "create_timeline")
    Data_encoding.title_encoding title
    ApiData.str_api_result_encoding cont
