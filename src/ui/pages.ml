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
  try
    match Hashtbl.find pages path with
    | exception Not_found -> set_in_main_page [error_404 ~path ~args ()]; finish ()
    | f ->
      let url = Ui_utils.url path args in
      Ui_utils.push url;
      f ~args
  with exn ->
    Js_utils.log "Exception in dispatch of %s: %s"
      path
      (Printexc.to_string exn);
    raise exn

let () = Dispatcher.dispatch := dispatch

let main_page ~args =
  Request.timeline_data ~args (fun events ->
      let page, init = Home.page args events in
      set_in_main_page [page];
      init ();
      finish ()
    )

let admin_page_if_trustworthy ~args =
  match List.assoc_opt "action" args with
  | Some "add" ->
    let update_action =
      fun event ->
        Js_utils.log "Adding event %a" Utils.pp_event event;
        ignore @@
        Request.add_event ~args
          event
          (function
            | Ok () -> dispatch ~path:"" ~args
            | Error s -> Lwt.return (Error (Xhr_lwt.Str_err ("Add new event action failed: " ^ s)))
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
        Request.events ~args
          (fun events -> set_in_main_page (Admin.events_list events); finish ())
      | Some i ->
        begin
          try
            let i = int_of_string i in
            Request.categories (fun categories ->
                Request.event ~args i (fun e ->
                    let update_action = (
                      fun new_event ->
                        Js_utils.log "Update...";
                        Request.update_event ~args i new_event (
                          function
                          | Ok () -> begin
                              Js_utils.log "Going back to main page";
                              dispatch
                                ~path:Admin.page_name
                                ~args:(["action", "edit"])
                            end
                          | Error s -> begin
                              Js_utils.log "Update failed: %s" s;
                              Lwt.return
                                (Error (Xhr_lwt.Str_err ("Update event action failed: " ^ s)))
                            end
                        )
                    ) in
                    let remove_action i =
                      ignore @@
                      Request.remove_event
                        ~args
                        i
                        (fun _ -> finish ()) in
                    let form = Admin.event_form e i categories update_action remove_action
                    in
                    set_in_main_page [form];
                    finish ())
              )
          with
            Invalid_argument _ ->
            let msg = Format.sprintf "Invalid event id %s" i in
            set_in_main_page [error_404 ~msg ~path:Admin.page_name ~args ()]; finish ()
        end
    end
  | Some _ -> set_in_main_page [error_404 ~path:Admin.page_name ~args ()]; finish ()

let admin_page_if_not_trustworthy () =
  set_in_main_page [
    Admin.admin_page_login
      ~login_action:(fun log pwd ->
          ignore @@ Request.login log pwd (fun b ->
              if b then begin
                Js_utils.log "Login OK!@.";
                Ui_utils.(set_as_trustworthy @@ hash pwd);
                Js_utils.reload ()
              end else
                Js_utils.alert "Wrong login/password@.";
              finish ())
        )
      ~register_action:(fun log pwd ->
          ignore @@ Request.register_user log pwd (fun _ -> finish ()))
  ];
  finish ()

let admin_page ~args =
  if Ui_utils.is_trustworthy () then
    admin_page_if_trustworthy ~args
  else
    admin_page_if_not_trustworthy ()

let () =
  add_page Home.page_name  main_page;
  add_page Admin.page_name admin_page
