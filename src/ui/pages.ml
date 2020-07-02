open Ui_common

open Timeline_data

type dispatcher = args:(string * string) list -> (unit, string) result Lwt.t

let pages : (string, dispatcher) Hashtbl.t = Hashtbl.create 3

let add_page path f =
  Hashtbl.add pages path f
  

let finish () = Lwt.return (Ok ())

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
    Timeline_vue.(init ~on_page:No_timeline ~categories:[]);
    finish ()
  | Some tid ->
    Request.timeline_data ~args tid (fun events ->
      Request.title ~args tid (fun title ->
        let _, events = events in  
        let on_page =
          match title, events with
          | None, [] -> Timeline_vue.No_timeline
          | _ -> Timeline_vue.Timeline {title; events; name = tid} in
        let categories =
          List.fold_left
            (fun acc (_, {Data_types.group; _}) ->
               match group with
               | None -> acc
               | Some g -> Utils.StringSet.add g acc
            )
            Utils.StringSet.empty
            events in
        let categories =
          let in_args = Args.get_categories args in            
          Utils.StringSet.fold
            (fun s acc -> (s, List.mem s in_args) :: acc)
            categories
            [] in
        Timeline_vue.init ~categories ~on_page;
        finish ()
        )
      )

let view_page ~args =
  match Args.get_timeline args with
  | None -> View_vue.init None []; finish ()
  | Some tid ->
    Request.view ~args tid (fun (title, events) ->
      View_vue.init title events;
      finish ()
    )

let () =
  Dispatcher.dispatch := dispatch;
  add_page ""              home_page;
  add_page "home"          home_page;
  add_page "timeline"      timeline_page;
  add_page "timeline/"     timeline_page;
  add_page "view"          view_page;
  add_page "view/"          view_page
