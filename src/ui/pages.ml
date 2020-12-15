open Ui_common

type dispatcher = args:(string * string) list -> (unit, string) result Lwt.t

let pages : (string, dispatcher) Hashtbl.t = Hashtbl.create 3

let add_page path f =
  Hashtbl.add pages path f
  

let finish e = Lwt.return (Ok e)

let rec dispatch ~path ~args =
  Js_utils.log "Path = %s" path;
  try
    match Hashtbl.find pages path with
    | exception Not_found -> dispatch ~path:"" ~args
    | init -> init ~args
  with exn ->
    Js_utils.log "Exception in dispatch of %s: %s"
      path
      (Printexc.to_string exn);
    raise exn

let home_page ~args:_ =
  Js_utils.log "Loading home page";
  Home_vue.init ();
  finish ()

let timeline_page ~args =
  Js_utils.log "Loading timeline page";
  match Args.get_timeline args with
  | None ->
    Js_utils.log "No id found";
    Timeline_vue.(init ~args ~on_page:(No_timeline {name = ""; id = ""}) ~categories:[] ~tokens:[]);
    finish ()
  | Some tid ->
    Js_utils.log "Id: %s" tid;
    let _name, tid = Ui_utils.timeline_id_from_arg tid in
    Request.get_tokens tid (fun tokens ->
      Request.timeline_name tid (fun name ->
        Request.timeline_data ~args tid (function
        | Ok (Timeline {title; events; edition_rights}) -> begin
          let name = Ui_utils.unURLize name in
          Js_utils.alert name;
          if edition_rights then begin
            Request.categories tid (fun categories ->
              let on_page =
                match title, events with
                | None, [] -> Timeline_vue.No_timeline {name; id=tid}
                | _ -> Timeline_vue.Timeline {title; events; name; id=tid} in
              let categories =
                let in_args = Args.get_categories args in            
                List.fold_left
                  (fun acc s -> (s, List.mem s in_args) :: acc)
                  []
                  categories in
              Timeline_vue.init ~args ~categories ~on_page ~tokens;
              finish ()
              )
          end else begin
            Ui_utils.goto_page (Format.sprintf "/view?timeline=%s-%s" name tid);
            finish ()
          end
        end
        | Ok NoTimeline -> begin
            Js_utils.alert "This timeline seems to have been deleted.";
            Timeline_cookies.remove_timeline tid;
            Ui_utils.goto_page "/";
            finish ()        
          end
        | Error s -> 
          Js_utils.alert (Format.sprintf "ERROR: API Server seems to be down: %s" s);
          Ui_utils.goto_page "/";
          finish ()
              )
          )
      )

let view_page ~args =
  match Args.get_timeline args with
  | None -> View_vue.init None "" None []; finish ()
  | Some tid ->
    let name, tid' = Ui_utils.timeline_id_from_arg tid in
    Request.timeline_data ~args tid' (function
      | Error s ->
        Js_utils.alert (Format.sprintf "ERROR: API Server seems to be down: %s" s);
        Ui_utils.goto_page "/";
        finish ()
      | Ok (Timeline {title; events; edition_rights}) ->
        let want_to_edit () =
          Js_utils.confirm
            "You have been granted edition rights. Do you wish to go to the edition page?" in
        let () =
          if edition_rights && want_to_edit () then
            Ui_utils.goto_page (Format.sprintf "/edit?timeline=%s-%s" name tid')
          else 
            View_vue.init (Some tid) name title events in
        finish ()
      | Ok NoTimeline -> 
        Js_utils.alert "This timeline seems to have been deleted.";
        Timeline_cookies.remove_timeline tid';
        Ui_utils.goto_page "/";
        finish ()        
    )

let () =
  Dispatcher.dispatch := dispatch;
  add_page ""              home_page;
  add_page "home"          home_page;
  add_page "edit"      timeline_page;
  add_page "edit/"     timeline_page;
  add_page "view"          view_page;
  add_page "view/"          view_page
