open Timeline_data
open Data_types
open Ui_common

exception IncorrectInput of string
let incorrect_input s = raise (IncorrectInput s)

let finish =
  function
  | Ok _ -> Lwt.return (Ok ())
  | Error s -> Alert_vue.alert ("Error: " ^s); Lwt.return (Ok ())

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
  Alert_vue.alert msg;
  Lwt.return (Error msg)

let error = custom_error None

let create_timeline ?email name descr cont =
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
  Request.create_timeline ~error ?email timeline_id title true
    ( fun (admin,_) ->
      cont ~name ~id:admin;
      finish @@ Ok ()
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
    cont
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
      event
      (fun s -> cont s; Lwt.return (Ok ()))
  with
    IncorrectInput s -> 
    Alert_vue.alert (Format.sprintf "Error: %s" s);
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
      Alert_vue.alert (Lang.t_ Text.s_alert_edition_conflict); Lwt.return (Ok ())
    )

let removeEvent ~id ~timeline_id =
  let open Lwt in
  Alert_vue.confirm (Lang.t_ Text.s_confirm_remove_event) >>= (fun confirm ->
  if confirm then
    Request.remove_event ~error ~id:(string_of_int id) ~timeline_id
      (fun () -> Js_utils.reload (); return (Ok ()))
  else return (Ok ()))

let export_timeline ?(name="timeline") title events =
  let title_line = match title with
    | None -> []
    | Some (_, t) -> Csv_utils.title_to_csv_line  t in
  let csv =
    title_line ::
    (List.map (fun (_, e) -> Csv_utils.title_to_csv_line (Utils.event_to_metaevent e)) events) in
  Ui_utils.download
    (name ^ ".csv")
    (Csv_utils.to_string csv)

let import_timeline tid is_public elt =
  let open Lwt in
  Js_utils.log "Importing timeline";
  Alert_vue.confirm "You are about to replace your timeline by the current one. Are you sure?" >>=
  (fun confirm ->
    if confirm then Lwt.return @@
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
               (fun () -> Alert_vue.alert "Success!"; Js_utils.reload (); finish (Ok ())) in ()
      )
    else Lwt.return false)

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

let removeTimeline tid cont =
  Request.remove_timeline tid
    (fun () -> cont (); finish (Ok ()))

let updateTimelineName tid new_name cont =
  Request.update_timeline_name new_name tid (fun failed -> cont failed; Lwt.return (Ok ()))
