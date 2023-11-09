open Js_of_ocaml.Url
open Lwt
open Timeline_data

module J = Json_schema

module ApiServices = Api_services.ApiServices

let get_service_path s = EzAPI.(Path.to_string (s.Service.path))

let api () =
  let h = {
    hu_host = !Config.API.api_host;
    hu_port = begin
      match !Config.API.api_root with
      | None -> !Config.API.api_port
      | Some _ -> 443
    end;
    hu_path = [];
    hu_path_string = "";
    hu_arguments = [];
    hu_fragment = "" } in
  Https h  (*
let api () =
  let h = {
    hu_host = "localhost";
    hu_port = 13579;
    hu_path = [];
    hu_path_string = "";
    hu_arguments = [];
    hu_fragment = "" } in
  Http h *)
(*
  match Js_of_ocaml.Url.Current.get () with
  | Some u -> u
  | None ->
    match Js_of_ocaml.Url.url_of_string "http://localhost:8080" with
    | Some u -> u
    | None -> assert false *)

let output_encodings_from_apifun apifun =
  let api_error_encodings = EzAPI.Service.errors apifun in
  let output_encoding =
    match EzAPI.Service.output apifun with
    | EzAPI.Service.IO.Json j -> j
    | _ -> assert false
  in
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
        (fun (EzAPI.Err.Case {encoding; select; deselect; _}) ->
           let encoding =
             Json_encoding.conv
               (fun x -> match select x with
                  | None -> assert false
                  | Some x -> x)
               deselect
               encoding
           in
           Json_encoding.(
             union [
               case
                 output_encoding
                 (function Ok s -> Some s | _ -> None)
                 (fun s -> Ok s);
               case
                 encoding
                 (function Error (_, s) -> Some s | _ -> None)
                 (fun s -> Error (500, s))
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

(* TODO: apiargs unused? Spurious!*)
let get
    ?(args = [])
    ~error
    (apifun : _ EzAPI.Service.t)
    (_apiargs : string list)
    (cont : 'output -> 'a)  =
  let url = api () in (*
  let url = (* Only for standalone !! *)
    match url wit<h
    | Http u -> Http {u with hu_path_string = ""}
    | Https u -> Https {u with hu_path_string = ""}
    | File f -> File {f with fu_path_string = ""} in *)
  let api_fun_name = get_service_path apifun in
  let () =
    Ezjs_tyxml.log "GET %s from %s with args [%a]"
      api_fun_name
      (Js_of_ocaml.Url.string_of_url url)
      (Format.pp_print_list
         ~pp_sep:(fun fmt _ -> Format.fprintf fmt "; ")
         (fun fmt (arg, value) -> Format.fprintf fmt "%s = %s" arg value)) args
  in
  let encodings = output_encodings_from_apifun apifun in
  Xhr_lwt.get ~args ~base:url api_fun_name encodings >>=
  (fun res ->
     Ezjs_tyxml.log "GET %s%s returned something" (Js_of_ocaml.Url.string_of_url url) api_fun_name;
     match res with
     | Ok elt -> begin
       Ezjs_tyxml.log "OK";
       match elt with
       | Ok res -> cont res
       | Error (code, e) -> error (Api (code, e))
     end
     | Error e ->
       Ezjs_tyxml.log "ERROR";
       let code, msg = Xhr_lwt.error_content e in
       Ezjs_tyxml.log "Error %i while getting to api: %s" code msg;
       error (Xhr (code, msg))
  )

(* TODO: apiargs unused? Spurious!*)
let post ~args ~error (apifun : _ EzAPI.Service.t) _apiargs input cont =
  let api_fun_name = EzAPI.(Path.to_string (apifun.Service.path)) in
  let () = Ezjs_tyxml.log "POST %s" api_fun_name in
  let url = api () in
  let () =
    Ezjs_tyxml.log "Calling API at %s -- %s" (Js_of_ocaml.Url.string_of_url url) api_fun_name in
  let input_encoding =
    match EzAPI.Service.input apifun with
    | Json e -> e
    | _ -> assert false
  in
  let output_encodings = output_encodings_from_apifun apifun in
  let () = Ezjs_tyxml.log "Encodings OK, calling POST" in
  Xhr_lwt.post
    ~eprint:Ezjs_tyxml.log ~args ~base:url input_encoding output_encodings api_fun_name input >>=
  (fun res ->
     Ezjs_tyxml.log "POST  %s%s returned something" (Js_of_ocaml.Url.string_of_url url) api_fun_name;
     match res with
     | Ok elt -> begin
       Ezjs_tyxml.log "OK";
       match elt with
       | Ok res -> cont res
       | Error (code, e) -> error (Api (code, e))
     end
     | Error e ->
       Ezjs_tyxml.log "ERROR";
       let code, msg = Xhr_lwt.error_content e in
       Ezjs_tyxml.log "Error %i while getting to api: %s" code msg;
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
    ~error:(fun _ -> cont (Error "[timeline_data] Error while reaching the API"))
    ~args
    ApiServices.timeline_data.EzAPI.s [timeline]
    (fun res -> cont (Ok res))

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
    ApiServices.add_event.EzAPI.s [tid]
    event
    cont

let update_event ~error ~id ~old_event ~new_event ~timeline_id cont =
  let args = args_from_session [] in
  post
    ~args
    ~error
    ApiServices.update_event.EzAPI.s []
    (id, old_event, new_event, timeline_id)
    cont

let remove_event ~error ~id ~timeline_id cont =
  let args = args_from_session ["event_id", id] in
  get
    ~error
    ~args
    ApiServices.remove_event.EzAPI.s [timeline_id]
    cont

let remove_timeline ~id cont =
  post
    ~error:(fun e -> return (Error e))
    ~args:(args_from_session [])
    ApiServices.remove_timeline.EzAPI.s [id]
    cont

let register_user email password cont =
  Ezjs_tyxml.log "Request register_user@.";
  let hash = Ui_utils.hash password (* todo: change this *) in
  Ezjs_tyxml.log "Hash: %s@." hash;
  post
    ~args:[]
    ApiServices.register_user.EzAPI.s []
    (email, hash)
    cont

let login email password cont =
  let hash = Ui_utils.hash password (* todo: change this *) in
  Ezjs_tyxml.log "Hash: %s@." hash;
  post
    ~args:[]
    ApiServices.login.EzAPI.s []
    (email, hash)
    cont

let is_auth cont =
  post
    ~args:(args_from_session [])
    ApiServices.is_auth.EzAPI.s []
    ()
    cont

let has_admin_rights timeline cont =
  post
    ~args:(args_from_session [])
    ApiServices.has_admin_rights.EzAPI.s [timeline]
    ()
    cont

let categories timeline cont =
  post
    ~error:(fun _e -> return (Error "Failed to get categories"))
    ~args:(args_from_session [])
    ApiServices.categories.EzAPI.s [timeline]
    ()
    cont

let logout ~error cont =
  match Ui_utils.get_auth_data () with
  | None -> Lwt.return @@ Error (Xhr_lwt.Str_err "Error: not logged in")
  | Some (email, auth_data) ->
    post
      ~error
      ~args:[]
      ApiServices.logout.EzAPI.s []
      (email, auth_data)
      cont

let create_timeline ?email timeline_id title is_public cont =
  let args =
    match email with
    | None -> []
    | Some email ->
      match Ezjs_lang.get () with
      | None -> ["email", email]
      | Some l -> ["email", email; "lang", l] in
  post
    ~args:(args_from_session args)
    ApiServices.create_timeline.EzAPI.s [timeline_id]
    (title, is_public)
    cont

let user_timelines cont =
  post
    ~args:(args_from_session [])
    ApiServices.user_timelines.EzAPI.s []
    ()
    cont

let allow_user user timeline cont =
  post
    ~args:(args_from_session [])
    ApiServices.allow_user.EzAPI.s []
    (user, timeline)
    cont

let timeline_users timeline cont =
  post
    ~args:(args_from_session [])
    ApiServices.timeline_users.EzAPI.s [timeline]
    ()
    cont

let remove_user cont =
  post
    ~args:(args_from_session [])
    ApiServices.remove_user.EzAPI.s []
    ()
    cont

let remove_timeline timeline cont =
  post
    ~error:(fun e -> return (Error e))
    ~args:(args_from_session [])
    ApiServices.remove_timeline.EzAPI.s [timeline]
    ()
    cont

let update_token_readonly ~error readonly timeline token cont =
  post
    ~error
    ~args:(args_from_session ["readonly", string_of_bool readonly])
    ApiServices.update_token_readonly.EzAPI.s [token]
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
    ApiServices.update_token_pretty.EzAPI.s [token]
    timeline
    cont

let remove_token ~error timeline token cont =
  post
    ~error
    ~args:(args_from_session [])
    ApiServices.remove_token.EzAPI.s [token]
    timeline
    cont

let create_token ~error args timeline cont =
  post
    ~error
    ~args:(args_from_session args)
    ApiServices.create_token.EzAPI.s [timeline]
    ()
    cont

let get_tokens timeline cont =
  post
    ~error:(fun _ -> cont [])
    ~args:[]
    ApiServices.get_tokens.EzAPI.s [timeline]
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
    ApiServices.import_timeline.EzAPI.s [timeline]
    (title, events, is_public)
    cont

let timeline_name (tid : string) cont =
  get
    ~error:(fun _ -> cont "Timeline")
    ~args:[]
    ApiServices.timeline_name.EzAPI.s [tid]
    cont

let update_timeline_name (new_name : string) (tid : string) cont =
  let args = args_from_session ["pretty", new_name] in
  post
    ~error:(fun _ -> Ezjs_tyxml.alert "Failed while updating timeline"; cont false)
    ~args
    ApiServices.update_timeline_name.EzAPI.s []
    tid
    (fun () -> cont true)
