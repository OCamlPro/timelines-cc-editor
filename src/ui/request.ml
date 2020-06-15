open Js_of_ocaml.Url
open Lwt
open EzAPI

let api () =
  Http {
    hu_host = "d4.dune.network";
    hu_port = Config.api_port;
    hu_path = [];
    hu_path_string = "";
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

let cook ?error encoding cont =
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
         match error with
         | None -> raise e
         | Some e -> e
     in
     cont elt
  )

let get ?error ?(args = []) (apifun : _ EzAPI.service) apiargs cont =
  let url = api () in (*
  let url = (* Only for standalone !! *)
    match url wit<h
    | Http u -> Http {u with hu_path_string = ""}
    | Https u -> Https {u with hu_path_string = ""}
    | File f -> File {f with fu_path_string = ""} in *)
  let api_fun_name = EzAPI.get_service_path apifun apiargs in
  let () =
    Js_utils.log "GET %s from %s with args [%a]"
      api_fun_name
      (Js_of_ocaml.Url.string_of_url url)
      (Format.pp_print_list
         ~pp_sep:(fun fmt _ -> Format.fprintf fmt "; ")
         (fun fmt (arg, value) -> Format.fprintf fmt "%s = %s" arg value)) args
  in
  Xhr_lwt.get ~args ~base:url api_fun_name >>=
  function
    Ok elt ->
    Js_utils.log "GET %s OK: %s" api_fun_name elt;
    let enc = EzAPI.service_output apifun in
    cook ?error enc cont elt
  | Error e ->
    let code, msg = Xhr_lwt.error_content e in
    Js_utils.log "Error %i while getting to api: %s" code msg;
    match error with
      None -> Lwt.return (Error e)
    | Some e -> cont e

let post ~args (apifun : _ EzAPI.service) apiargs input cont =
  let api_fun_name = EzAPI.get_service_path apifun apiargs in
  let () = Js_utils.log "POST %s" api_fun_name in
  let url = api () in
  let () =
    Js_utils.log "Calling API at %s -- %s" (Js_of_ocaml.Url.string_of_url url) api_fun_name in
  let input_encoding = EzAPI.service_input apifun in
  let output_encoding = EzAPI.service_output apifun in
  let output_encodings = [
    output_encoding;
    Json_encoding.tup1 output_encoding
  ] in    
  Xhr_lwt.post
    ~eprint:Js_utils.log ~args ~base:url input_encoding output_encodings api_fun_name input >>=
  (fun res ->
     Js_utils.log "POST  %s%s returned something" (Js_of_ocaml.Url.string_of_url url) api_fun_name;
     match res with
     | Ok elt -> cont elt
     | Error e ->
       let code, msg = Xhr_lwt.error_content e in
       Js_utils.log "Error %i while getting to api: %s" code msg;
       Lwt.return (Error e)
  )


let args_from_session args =
  match Ui_utils.get_auth_data () with
  | None -> args
  | Some (email, auth_data) ->
    ("auth_email", email) :: ("auth_data", auth_data) :: args

let timeline_data ~args timeline cont =
  let args = args_from_session args in
  get
    ~error:(Error "GET timeline_data failed")
    ~args
    ApiServices.timeline_data [timeline]
    cont

let event ~args (id : int) cont =
  let args = args_from_session args in
  get
    ~args
    ApiServices.event [string_of_int id]
    cont

let events ~args (tid : string) cont =
  let args = args_from_session args in
  get
    ~args
    ApiServices.events [tid]
    cont

let title ~args tid cont =
  get
    ~args
    ApiServices.title [tid]
    cont

let add_event (tid : string) (event : Data_types.event) cont =
  let args = args_from_session [] in
  post
    ~args
    ApiServices.add_event [tid]
    event
    cont

let update_event id ~old_event ~new_event cont =
  let args = args_from_session ["id", string_of_int id] in
  post
    ~args
    ApiServices.update_event []
    (id, old_event, new_event)
    cont

let remove_event id cont =
  let args = args_from_session [] in
  get
    ~args
    ApiServices.remove_event [id]
    cont

let register_user email password cont =
  Js_utils.log "Request register_user@.";
  let hash = Ui_utils.hash password (* todo: change this *) in
  Js_utils.log "Hash: %s@." hash;
  post
    ~args:[]
    ApiServices.register_user []
    (email, hash)
    cont

let login email password cont =
  let hash = Ui_utils.hash password (* todo: change this *) in
  Js_utils.log "Hash: %s@." hash;
  post
    ~args:[]
    ApiServices.login []
    (email, hash)
    cont

let is_auth cont =
  post
    ~args:(args_from_session [])
    ApiServices.is_auth []
    ()
    cont

let has_admin_rights timeline cont =
  post
    ~args:(args_from_session [])
    ApiServices.has_admin_rights [timeline]
    ()
    cont

let categories timeline cont =
  post
    ~args:(args_from_session [])
    ApiServices.categories [timeline]
    ()
    cont

let logout cont =
  match Ui_utils.get_auth_data () with
  | None -> Lwt.return @@ Error (Xhr_lwt.Str_err "Error: not logged in")
  | Some (email, auth_data) ->
    post
      ~args:[]
      ApiServices.logout []
      (email, auth_data)
      cont

let create_timeline timeline_id title is_public cont =
  post 
    ~args:(args_from_session [])
    ApiServices.create_timeline [timeline_id]
    (title, is_public)
    cont

let user_timelines cont =
  post 
    ~args:(args_from_session [])
    ApiServices.user_timelines []
    ()
    cont

let allow_user user timeline cont =
  post 
    ~args:(args_from_session [])
    ApiServices.allow_user []
    (user, timeline)
    cont

let timeline_users timeline cont = 
  post 
    ~args:(args_from_session [])
    ApiServices.timeline_users [timeline]
    ()
    cont

let remove_user timeline cont = 
  post 
    ~args:(args_from_session [])
    ApiServices.remove_user []
    ()
    cont

let remove_timeline timeline cont = 
  post 
    ~args:(args_from_session [])
    ApiServices.remove_timeline [timeline]
    ()
    cont

let get_view_token timeline cont =
  get
    ~args:[]
    ApiServices.get_view_token [timeline]
    cont

let view ~args timeline cont =
  get
    ~error:(Error "GET view failed")
    ~args
    ApiServices.view [timeline]
    cont
