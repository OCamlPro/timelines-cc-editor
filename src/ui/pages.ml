open Js_utils
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Controller

type dispatcher = unit -> (unit, unit Xhr_lwt.error) result Lwt.t

let pages : (string, dispatcher) Hashtbl.t = Hashtbl.create 3

let add_page path f = Hashtbl.add pages path f

let finish () = Lwt.return (Ok ())

let rec dispatch ~path =
  try
    match Hashtbl.find pages path with
    | exception Not_found -> dispatch ~path:""
    | init -> init ()
  with exn ->
    Js_utils.log "Exception in dispatch of %s: %s"
      path
      (Printexc.to_string exn);
    raise exn

let home_page () =
  Home_vue.init ();
  finish ()

let timeline_page () =
  let args = Jsloc.args () in
  match Args.get_timeline args with
  | None ->
    Timeline_vue.(init ~on_page:No_timeline ~categories:[]);
    finish ()
  | Some tid ->
    Request.timeline_data ~args tid (fun data ->
        let title, events =
          match data with
          | Error s -> Js_utils.alert s;
            None, []
          | Ok t -> t in
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

let () =
  add_page ""              home_page;
  add_page "home"          home_page;
  add_page "timeline"      timeline_page
