open Js_utils
open Js_of_ocaml_tyxml.Tyxml_js.Html

type dispatcher = args:(string * string) list -> (unit, unit Xhr_lwt.error) result Lwt.t

let pages : (string, dispatcher) Hashtbl.t = Hashtbl.create 3

let add_page path f = Hashtbl.add pages path f

let main_div_id = "page-content"
let get_main_page () = find_component main_div_id
let set_in_main_page content = Manip.replaceChildren (get_main_page ()) content

let default_page ?(link_name = "Home") ~classes () =
  Ui_utils.a_link
    ~args:[]
    ~path:""
    ~classes:("border" :: classes)
    [txt link_name]

let error_404 ?(msg="Unknown page") ~path ~args () =
  div ~a:[a_class ["center"; "big-block"]] [
    h2 [txt "Error 404"];
    br ();
    txt @@ Format.sprintf "It seems you are lost. %s" msg;
    br ();
    default_page ~classes:["center"] ()
  ]

let finish () = Lwt.return (Ok ())

let dispatch ~path ~args =
  (*Misc_js.UpdateOnFocus.incr_page ();*)
  try
    match Hashtbl.find pages path with
    | exception Not_found -> set_in_main_page [error_404 ~path ~args ()]; finish ()
    | f -> f ~args
  with exn ->
    Js_utils.log "Exception in dispatch of %s: %s"
      path
      (Printexc.to_string exn);
    raise exn

let () = Dispatcher.dispatch := dispatch

let main_page ~args =
  Request.timeline_data
    (fun json ->
       let json = Format.sprintf "{\"events\":%s}" json in
       let cmd =
         Format.asprintf "window.timeline = new TL.Timeline('timeline-embed',%s)" json in
       let () = Js_of_ocaml.Js.Unsafe.js_expr cmd in
       finish ()
    )

let admin_page ~args =
  match List.assoc_opt "action" args with
  | Some "add" ->
    let update_action =
      fun event ->
        Js_utils.log "Adding event %a" Utils.pp_event event;
        ignore @@
        Request.add_event
          event
          (fun b ->
             if b then
               dispatch ~path:"" ~args:[]
             else Lwt.return (Error (Xhr_lwt.Str_err "Add new event action failed"))
          ) in
    let remove_action = fun _ -> () in
    Request.categories (fun categories ->
        set_in_main_page [Admin.add_new_event_form categories update_action remove_action];
        finish ()
      )
  | None | Some "edit" ->
    begin
      match List.assoc_opt "id" args with
      | None ->
        Request.events
          (fun events -> set_in_main_page (Admin.events_list events); finish ())
      | Some i ->
        begin
          try
            let i = int_of_string i in
            Request.categories (fun categories ->
                Request.event i (fun e ->
                    let update_action = (
                      fun new_event ->
                        Js_utils.log "Update...";
                        Request.update_event i new_event (
                          fun b ->
                            Js_utils.log "Update OK";
                            if b then begin
                              Js_utils.log "Going back to main page";
                              dispatch ~path:"admin" ~args:["action", "edit"]
                            end else begin
                              Js_utils.log "Update failed";
                              Lwt.return
                                (Error (Xhr_lwt.Str_err "Update event action failed"))
                            end
                        )
                    ) in
                    let remove_action i = ignore @@ Request.remove_event i (fun _ -> finish ()) in
                    let form = Admin.event_form e i categories update_action remove_action
                    in
                    set_in_main_page [form];
                    finish ())
              )
          with
            Invalid_argument _ ->
            let msg = Format.sprintf "Invalid event id %s" i in
            set_in_main_page [error_404 ~msg ~path:"admin" ~args ()]; finish ()
        end
    end
  | Some _ -> set_in_main_page [error_404 ~path:"admin" ~args ()]; finish ()

let () =
  add_page ""      main_page;
  add_page "admin" admin_page
