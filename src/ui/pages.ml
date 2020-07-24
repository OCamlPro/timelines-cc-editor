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
      Request.timeline_data ~args tid (fun (title, events) ->
        Request.timeline_name tid (fun name ->
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
              )
          )
      )

let view_page ~args =
  match Args.get_timeline args with
  | None -> View_vue.init None "" None []; finish ()
  | Some tid ->
    let name, tid' = Ui_utils.timeline_id_from_arg tid in
    Request.timeline_data ~args tid' (fun (title, events) ->
      View_vue.init (Some tid) name title events;
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
