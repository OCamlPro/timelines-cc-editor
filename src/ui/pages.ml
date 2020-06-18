open Js_utils
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Controller

type dispatcher = args:(string * string) list -> (unit, unit Xhr_lwt.error) result Lwt.t

let pages : (string, dispatcher) Hashtbl.t = Hashtbl.create 3

let add_page path f = Hashtbl.add pages path f

let finish () = Lwt.return (Ok ())

let rec dispatch ~path ~args =
  try
    match Hashtbl.find pages path with
    | exception Not_found -> dispatch ~path:"" ~args
    | init -> init args
  with exn ->
    Js_utils.log "Exception in dispatch of %s: %s"
      path
      (Printexc.to_string exn);
    raise exn

let home_page ~args:_ =
  Home_vue.init ();
  finish ()

let timeline_page ~args =
  match Args.get_timeline args with
  | None ->
    Timeline_vue.(init ~on_page:No_timeline ~categories:[]);
    finish ()
  | Some tid ->
    Request.timeline_data ~args tid (fun data ->
      Request.title ~args tid (fun title ->
        let _, events =
          match data with
          | Error s -> Js_utils.alert s;
            None, []
          | Ok t -> t in
        let title =
          match title with
          | Error _ -> None
          | Ok t -> Some t in
        let on_page =
          match data with
          | Error _ -> Timeline_vue.No_timeline
          | Ok t -> Timeline_vue.Timeline {title; events; name = tid} in
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
    Request.view ~args tid (function
      | Ok (title, events) ->
        View_vue.init title events;
        finish ()
      | Error s ->
        Js_utils.alert ("Error while requesting view: " ^ s);
        View_vue.init None [];
        finish ()
      )

let () =
  Dispatcher.dispatch := dispatch;
  add_page ""              home_page;
  add_page "home"          home_page;
  add_page "timeline"      timeline_page;
  add_page "view"          view_page
