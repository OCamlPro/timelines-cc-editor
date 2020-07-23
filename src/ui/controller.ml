open Timeline_data
open Data_types
open Ui_common

exception IncorrectInput of string
let incorrect_input s = raise (IncorrectInput s)

let finish =
  function
  | Ok _ -> Lwt.return (Ok ())
  | Error s -> Js_utils.alert ("Error: " ^s); Lwt.return (Ok ())

let custom_error default err =
  let pp code msg = Format.asprintf "Error %i: %s" code msg in
  let msg =
    match err with
    | Request.Xhr (code, msg) -> pp code msg
    | Api (code, msg) -> begin
        match default with
        | None -> pp code msg
        | Some m -> m
      end
  in
  Js_utils.alert msg;
  Lwt.return (Error msg)

let error = custom_error None

let create_timeline name descr =
  let timeline_id, headline, name =
    match name with
    | "" ->
      Random.self_init ();
      let i1 = Random.bits () |> string_of_int in
      let i2 = Random.bits () |> string_of_int in
      i1 ^ i2, Lang.t_ Text.s_default_title, None
    | _ -> name, name, Some name
  in
  let title = Utils.to_title_event headline descr in
  let error e = Lwt.return @@ Error e in 
  Request.create_timeline ~error timeline_id title true (
    function id ->
      let () =
        let name =
          match name with
          | None -> id
          | Some n -> n in
        Timeline_cookies.add_timeline name id false in
      let id = Ui_utils.timeline_arg_from_id ?name id in
      let new_page = Format.sprintf "/edit?timeline=%s" id in
      Js_utils.log "Going to %s" new_page;
      finish @@ Ok (Ui_utils.goto_page new_page)
    )

let add_event
    ~start_date
    ~end_date
    ~media
    ~headline
    ~text
    ~unique_id
    ~group
    ~ponderation
    ~confidential
    ~tags
    ~timeline
  =
  try
    let start_date =
      match start_date with
      | None -> CalendarLib.Date.today ()
      | Some d -> d in
    let unique_id =
      match unique_id with
      | "" ->
        if headline = "" then
          incorrect_input "Headline & unique-id cannot be empty at the same time"
        else headline
      | _ -> unique_id in
    let tags = String.split_on_char ',' (Utils.trim tags) in
    let media =
      match media with
      | "" -> None
      | url -> Some {url} in
    let group =
      match group with
      | "" -> None
      | _ -> Some group in
    let event = {
      start_date;
      end_date;
      media;
      text = {headline; text};
      unique_id;
      group;
      ponderation;
      confidential;
      last_update = Some (CalendarLib.Date.today ());
      tags
    } in
    Request.add_event
      ~error timeline
      event (fun _ -> Js_utils.reload (); Lwt.return (Ok ()))
  with
    IncorrectInput s -> 
    Js_utils.alert (Format.sprintf "Error: %s" s);
    Lwt.return (Error s)

let update_event
    ~id
    ~old_event
    ~start_date
    ~end_date
    ~media
    ~headline
    ~text
    ~unique_id
    ~group
    ~ponderation
    ~confidential
    ~tags
    ~timeline_id =
  let unique_id =
    match unique_id with
    | "" ->
      if headline = "" then
        incorrect_input "Headline & unique-id cannot be empty at the same time"
      else headline
    | _ -> unique_id in
  let tags = String.split_on_char ',' (Utils.trim tags) in
  let media =
    match media with
    | "" -> None
    | url -> Some {url} in
  let group =
    match group with
    | "" -> None
    | _ -> Some group in
  let new_event = {
    start_date;
    end_date;
    media;
    text = {headline; text};
    unique_id;
    group;
    ponderation;
    confidential;
    last_update = Some (CalendarLib.Date.today ());
    tags
  } in
  Request.update_event
    ~error ~id
    ~old_event ~new_event
    ~timeline_id (function
    | Success ->
      Js_utils.reload (); Lwt.return (Ok ())
    | Modified _t ->
      Js_utils.alert (Lang.t_ Text.s_alert_edition_conflict); Lwt.return (Ok ())
    )

let removeEvent ~id ~timeline_id =
  if Js_utils.confirm (Lang.t_ Text.s_confirm_remove_event) then
    Request.remove_event ~error ~id:(string_of_int id) ~timeline_id
      (fun () -> Js_utils.reload (); Lwt.return (Ok ()))
  else Lwt.return (Ok ())

let export_timeline title events =
  let title_line = match title with
    | None -> []
    | Some (_, t) -> Csv_utils.title_to_csv_line  t in
  let csv =
    title_line ::
    (List.map (fun (_, e) -> Csv_utils.title_to_csv_line (Utils.event_to_metaevent e)) events) in
  Ui_utils.download "timeline.csv" (Csv_utils.to_string csv)

let import_timeline tid is_public elt =
  Js_utils.log "Importing timeline";
  if Js_utils.confirm "You are about to replace your timeline by the current one. Are you sure?" then
  Js_utils.Manip.upload_input ~btoa:false ~encoding:"UTF-8" elt
    (fun file_content ->
       let title, events = Csv_utils.from_string file_content in
       let title =
         match title with
         | None -> Utils.to_title_event "Title" "Text"
         | Some t -> t in
       let _lwt =
         Request.import_timeline
           ~error:error
           ~args:[]
           tid title
           events is_public
           (fun () -> Js_utils.alert "Success!"; Js_utils.reload (); finish (Ok ())) in () 
    )
    else false

let addToken
  ~readonly
  ?pretty
  tid
  with_tokens =
  let args =
    ("readonly", string_of_bool readonly) :: Args.get_args () in
  let args =
    match pretty with
    | None -> args
    | Some p -> ("pretty", p) :: args in
  Request.create_token ~error args tid
    (fun _str ->
      Request.get_tokens tid (
        fun tokens -> 
          with_tokens tokens; 
          Lwt.return (Ok ())
      )
    )

let updateTokenFilter ~readonly tid token with_tokens =
  Request.update_token_readonly ~error readonly tid token (
    fun () ->
      Request.get_tokens tid (
        fun tokens -> 
          with_tokens tokens; 
          Lwt.return (Ok ())
      )
    )

let updateTokenName pretty tid token with_tokens =
  Request.update_token_pretty ~error pretty tid token (
    fun () ->
      Request.get_tokens tid (
        fun tokens -> 
          with_tokens tokens; 
          Lwt.return (Ok ())
      )
    )

let removeToken tid token with_tokens =
  Request.remove_token ~error tid token 
    (fun () -> 
      Request.get_tokens tid (
        fun tokens -> 
          with_tokens tokens; 
          Lwt.return (Ok ())
      )
    )

let removeTimeline tid =
  Request.remove_timeline tid
    (fun _ ->
       Timeline_cookies.remove_timeline tid;
       Ui_utils.goto_page "/";
       finish (Ok ())
    )

(*open Data_types

let finish () = Lwt.return (Ok ())

let error s = Lwt.return (Error (Xhr_lwt.Str_err ("Add new event action failed: " ^ s)))

let timeline_id_from_args = List.assoc_opt "timeline"

let login log pwd =
  ignore @@
  Request.login log pwd (function
    | Some auth_data -> begin
        Js_utils.log "Login OK!@.";
        Ui_utils.auth_session log auth_data;
        Js_utils.reload ();
        finish ()
      end
    | None -> begin
        Js_utils.alert "Wrong login/password@.";
        error ("Wrong login")
      end)

let logout () =
  ignore @@
  Request.logout
    (fun _ ->
       Ui_utils.logout_session ();
       !Dispatcher.dispatch ~path:"" ~args:[] ()
    )

let register_account log pwd =
  ignore @@
  Request.register_user log pwd (function
      | Ok () ->
        Js_utils.alert "Account successfully registered! You can now log in.";
        finish ()
      | Error e ->
        Js_utils.alert ("Error: " ^e);
        error e
    )

let add_action args timeline event =
  match Utils.metaevent_to_event event with
  | None ->
    Js_utils.alert "Start date is missing";
    Lwt.return (Error (Xhr_lwt.Str_err "Start date is missing"));
  | Some event ->
    Js_utils.log "Adding event %a" Utils.pp_event event;
    match timeline_id_from_args args with
    | None ->
      Lwt.return (Error (Xhr_lwt.Str_err ("Add new event action failed: no timeline specified")))
    | Some timeline ->
      Request.add_event ~args timeline event
        (function
          | Ok s ->
            Js_utils.log "Event added";
            !Dispatcher.dispatch ~path:"home" ~timeline ~args ()
          | Error s ->
            let err = "Add new event action failed: " ^ s in
            Js_utils.alert err;
            Lwt.return (Error (Xhr_lwt.Str_err err))
        )

let remove_action args i =
  let c = Js_utils.confirm "Are you sure you want to remove this event ? This is irreversible." in
  if c then
    ignore @@
    Request.remove_event
      ~args
      i
      (fun _ ->
         ignore @@ !Dispatcher.dispatch ~path:(Ui_utils.get_path ()) ~args:[] ();
         finish ())
  else ()

let rec update_action
    (compare :
       int -> string list ->
     date option meta_event option -> date option meta_event -> 'a Ocp_js.elt)
    (i : int)
    (categories : string list)
    (old_event : title)
    (new_event : title)
    cont =
  Js_utils.log "Update... %a -> %a" Utils.pp_title old_event Utils.pp_title new_event;
  begin
    Request.update_event i ~old_event ~new_event (
      function
      | Success -> cont ()
      | Failed s -> begin
          Js_utils.log "Update failed: %s" s;
          Lwt.return
            (Error (Xhr_lwt.Str_err ("Update event action failed: " ^ s)))
        end
      | Modified event_opt ->
        Js_utils.log "Event has been modified while editing";
        Dispatcher.set_in_main_page [
          compare
            i
            categories
            event_opt
            new_event
        ];
        finish ()
    )
  end

let export_database args =
  match timeline_id_from_args args with
  | None ->
    Lwt.return (Error (Xhr_lwt.Str_err ("Export database failed: no timeline specified")))
  | Some timeline ->
    Request.events ~args timeline (fun events ->
      Request.title ~args timeline (fun title ->
        let sep = "," in
        let title =
          match title with
          | Error _ -> sep
          | Ok (_, title) -> Data_encoding.title_to_csv ~sep title in
        let header = Data_encoding.header ~sep in
        let events =
          List.fold_left
            (fun acc event ->
               acc ^ Data_encoding.event_to_csv ~sep event ^ ";\n")
            ""
            (snd @@ List.split events) in
        let str =  (title ^ ";\n" ^ header ^ ";\n" ^ events) in
        Ui_utils.download "database.csv" str; finish ()
          )
      )

let create_timeline = Request.create_timeline

let user_timelines = Request.user_timelines

let allow_user user timeline =
  ignore @@
  Request.allow_user user timeline
    (function 
     | Ok () -> (* todo: check why this branch is always taken *)
       Js_utils.log "User allowed";
       Lwt.return (Ok ())
     | Error s -> 
       Js_utils.alert (Format.sprintf "Error while adding user: %s" s);
       Lwt.return (Ok ())
    )

let goto_selection () =
  Dispatcher.validate_dispatch @@
  !Dispatcher.dispatch ~path:"" ~args:[] ()

let remove_timeline timeline =
  if Js_utils.confirm "Are you sure you want to delete your timeline? Everything will be lost!" then
    ignore (
      Request.remove_timeline timeline (
        function 
          | Ok () -> goto_selection (); Lwt.return (Ok ())
          | Error s -> 
            Js_utils.alert "Error while deleting timeline."; 
            Lwt.return (Error (Xhr_lwt.Str_err s))
       )
    )

let remove_account () =
  if
    Js_utils.confirm 
      "Are you sure you want to delete your account? \
       Every timeline you exclusively own will be deleted and you will lose all \
       your admin priviledges." then
  ignore @@ 
  Request.remove_user () (
    function 
      | Ok () -> logout (); Lwt.return (Ok ())
      | Error s -> 
        Js_utils.alert "Error while deleting account."; 
        Lwt.return (Error (Xhr_lwt.Str_err s))
    )
*)
