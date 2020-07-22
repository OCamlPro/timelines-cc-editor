open Js_of_ocaml.Url
open Lwt
open Timeline_data

module ApiServices = Api_services.ApiServices

let api () =
  let h = {
    hu_host = Config.api_host;
    hu_port = begin
      match Config.api_root with
      | None -> Config.api_port
      | Some _ -> 443
    end;
    hu_path = [];
    hu_path_string = "";
    hu_arguments = [];
    hu_fragment = "" } in
  Https h
(*
let api () =
  let h = {
    hu_host = "localhost";
    hu_port = 13579;
    hu_path = [];
    hu_path_string = "";
    hu_arguments = [];
    hu_fragment = "" } in
  Http h
  *)
(*
  match Js_of_ocaml.Url.Current.get () with
  | Some u -> u
  | None ->
    match Js_of_ocaml.Url.url_of_string "http://localhost:8080" with
    | Some u -> u
    | None -> assert false *)

let output_encodings_from_apifun apifun =
  let possible_codes = [500] in
  let api_error_encodings =
    List.fold_left (
      fun acc code ->
        match EzAPI.service_errors apifun ~code with
        | None -> acc
        | Some enc -> (code, enc) :: acc
    ) [] possible_codes in
  let output_encoding = EzAPI.service_output apifun in
  let output_encodings =
    match api_error_encodings with
    | [] -> [
        Json_encoding.conv
          (function Ok x -> x | Error _ -> assert false)
          (fun x -> Ok x)
          output_encoding
      ]
    | _ ->
      List.map
        (fun (code, api_error) ->
           Json_encoding.(
             union [
               case
                 output_encoding
                 (function Ok s -> Some s | _ -> None)
                 (fun s -> Ok s);
               case
                 api_error
                 (function Error (_, s) -> Some s | _ -> None)
                 (fun s -> Error (code, s))
             ]
           )
        )
        api_error_encodings in
  List.flatten @@ List.map
    (fun enc -> [enc; Json_encoding.tup1 enc])
    output_encodings

type 'a error_kind =
  | Xhr of int * string
  | Api of int * 'a

let get
    ?(args = [])
    ~error
    (apifun : ('params, 'params2, 'input, 'output, 'error, 'security) EzAPI.service)
    (apiargs : string list)
    (cont : 'output -> 'a)  =
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
  let encodings = output_encodings_from_apifun apifun in
  Xhr_lwt.get ~args ~base:url api_fun_name encodings >>=
  (fun res ->
     Js_utils.log "GET %s%s returned something" (Js_of_ocaml.Url.string_of_url url) api_fun_name;
     match res with
     | Ok elt -> begin
       Js_utils.log "OK";
       match elt with
       | Ok res -> cont res
       | Error (code, e) -> error (Api (code, e))
     end
     | Error e ->
       Js_utils.log "ERROR";
       let code, msg = Xhr_lwt.error_content e in
       Js_utils.log "Error %i while getting to api: %s" code msg;
       error (Xhr (code, msg))
  )

let post ~args ~error (apifun : _ EzAPI.service) apiargs input cont =
  let api_fun_name = EzAPI.get_service_path apifun apiargs in
  let () = Js_utils.log "POST %s" api_fun_name in
  let url = api () in
  let () =
    Js_utils.log "Calling API at %s -- %s" (Js_of_ocaml.Url.string_of_url url) api_fun_name in
  let input_encoding = EzAPI.service_input apifun in 
  let output_encodings = output_encodings_from_apifun apifun in    
  Xhr_lwt.post
    ~eprint:Js_utils.log ~args ~base:url input_encoding output_encodings api_fun_name input >>=
  (fun res ->
     Js_utils.log "POST  %s%s returned something" (Js_of_ocaml.Url.string_of_url url) api_fun_name;
     match res with
     | Ok elt -> begin
       Js_utils.log "OK";
       match elt with
       | Ok res -> cont res
       | Error (code, e) -> error (Api (code, e))
     end
     | Error e ->
       Js_utils.log "ERROR";
       let code, msg = Xhr_lwt.error_content e in
       Js_utils.log "Error %i while getting to api: %s" code msg;
       error (Xhr (code, msg))
  )


let args_from_session args =
  match Ui_utils.get_auth_data () with
  | None -> args
  | Some (email, auth_data) ->
    ("auth_email", email) :: ("auth_data", auth_data) :: args

let timeline_data ~args timeline cont  =
  let args = args_from_session args in
  get
    ~error:(fun _ -> Js_utils.alert "GET timeline_data failed"; cont (None, []))
    ~args
    ApiServices.timeline_data [timeline]
    cont

(*
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
    cont *)

let add_event ~error (tid : string) (event : Data_types.event) cont =
  let args = args_from_session [] in
  post
    ~args
    ~error
    ApiServices.add_event [tid]
    event
    cont

let update_event ~error ~id ~old_event ~new_event ~timeline_id cont =
  let args = args_from_session ["id", string_of_int id] in
  post
    ~args
    ~error
    ApiServices.update_event []
    (id, old_event, new_event, timeline_id)
    cont

let remove_event ~error ~id ~timeline_id cont =
  let args = args_from_session ["event_id", id] in
  get
    ~error
    ~args
    ApiServices.remove_event [timeline_id]
    cont

let remove_timeline ~id cont =
  post
    ~error:(fun e -> return (Error e))
    ~args:(args_from_session [])
    ApiServices.remove_timeline [id]
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
    ~error:(fun _e -> return (Error "Failed to get categories"))
    ~args:(args_from_session [])
    ApiServices.categories [timeline]
    ()
    cont

let logout ~error cont =
  match Ui_utils.get_auth_data () with
  | None -> Lwt.return @@ Error (Xhr_lwt.Str_err "Error: not logged in")
  | Some (email, auth_data) ->
    post
      ~error
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

let remove_user cont = 
  post 
    ~args:(args_from_session [])
    ApiServices.remove_user []
    ()
    cont

let remove_timeline timeline cont = 
  post 
    ~error:(fun e -> return (Error e))
    ~args:(args_from_session [])
    ApiServices.remove_timeline [timeline]
    ()
    cont

let update_token_readonly ~error readonly timeline token cont =
  post
    ~error
    ~args:(args_from_session ["readonly", string_of_bool readonly])
    ApiServices.update_token_readonly [token]
    timeline
    cont

let update_token_pretty ~error pretty timeline token cont =
  let args =
    let l = args_from_session [] in 
    match pretty with
    | None -> l
    | Some p -> ("pretty", p) :: l in
  post
    ~error
    ~args
    ApiServices.update_token_pretty [token]
    timeline
    cont

let remove_token ~error timeline token cont =
  post
    ~error
    ~args:(args_from_session [])
    ApiServices.remove_token [token]
    timeline
    cont

let create_token ~error args timeline cont =
  post
    ~error
    ~args:(args_from_session args)
    ApiServices.create_token [timeline]
    ()
    cont

let get_tokens timeline cont =
  post
    ~error:(fun _ -> cont [])
    ~args:[]
    ApiServices.get_tokens [timeline]
    ()
    cont

let import_timeline
    ~error ~args
    (timeline : string) (title : Data_types.title)
    (events : Data_types.event list) (is_public : bool)
    cont =
  post
    ~error
    ~args
    ApiServices.import_timeline [timeline]
    (title, events, is_public)
    cont

let timeline_name (tid : string) cont =
  get
    ~error:(fun _ -> cont "Timeline")
    ~args:[]
    ApiServices.timeline_name [tid]
    cont
