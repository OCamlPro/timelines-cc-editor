open Lwt
open Data_types

exception IncorrectInput of string
let incorrect_input s = raise (IncorrectInput s)

let finish =
  function
  | Ok _ -> Lwt.return (Ok ())
  | Error s -> Js_utils.alert ("Error: " ^s); Lwt.return (Ok ())

let create_timeline name descr =
  let timeline_id =
    match name with
    | "" ->
      Random.self_init ();
      let i1 = Random.bits () |> string_of_int in
      let i2 = Random.bits () |> string_of_int in
      i1 ^ i2
    | _ -> name
  in
  let title = Utils.to_title_event name descr in
  let msg = Format.sprintf "You will create the timeline %s : %s, are you sure ?" timeline_id descr in
  if Js_utils.confirm msg then
    Request.create_timeline timeline_id title true (
      fun res ->
        let () = match res with
          | Error _->
            Js_utils.log "Error from timeline API"       
          (* Todo: better error message *)
          | Ok id ->
            let new_page = Format.sprintf "/timeline?timeline=%s" id in
            Js_utils.log "Going to %s" new_page;
            Ui_utils.goto_page new_page
        in finish res
    )
  else 
  Lwt.return (Ok ())

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
    Request.add_event timeline event (function
        | Ok _id -> Js_utils.reload (); Lwt.return (Ok ())
        | Error s ->
          Js_utils.alert (Format.sprintf "Error: %s" s);
          Lwt.return (Error (Xhr_lwt.Str_err s)))
  with
    IncorrectInput s -> 
    Js_utils.alert (Format.sprintf "Error: %s" s);
    Lwt.return (Error (Xhr_lwt.Str_err s))

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
    ~timeline =
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
  Request.update_event id ~old_event ~new_event (function
    | Success ->
      Js_utils.reload (); Lwt.return (Ok ())
    | Modified t ->
      Js_utils.alert "Todo: event has been modified while editing."; Lwt.return (Ok ())
    | Failed s -> Js_utils.alert s; Lwt.return (Error (Xhr_lwt.Str_err s))
    )

let removeEvent i =
  if Js_utils.confirm "Are you sure to remove this event? You cannot undo this." then
    Request.remove_event (string_of_int i) (function
        | Ok () -> Js_utils.reload (); Lwt.return (Ok ())
        | Error s -> Js_utils.alert ("Error while removing event: " ^ s); Lwt.return (Error (Xhr_lwt.Str_err s))
      )
  else Lwt.return (Ok ())
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
