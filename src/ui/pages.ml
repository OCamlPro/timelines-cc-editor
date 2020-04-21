open Js_utils
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Dispatcher
open Controller

type dispatcher = args:(string * string) list -> (unit, unit Xhr_lwt.error) result Lwt.t

let pages : (string, dispatcher) Hashtbl.t = Hashtbl.create 3

let add_page path f = Hashtbl.add pages path f

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
      Request.is_auth (fun is_auth ->
          Request.categories (fun categories ->
              let page, init =
                Home.page
                  ~login_action
                  ~register_action
                  ~logout_action
                  ~add_action
                  ~update_action:(update_action Admin.compare args)
                  is_auth args categories events in
              set_in_main_page [page];
              init ();
              finish ()
            )
        )
    )

let admin_page_if_trustworthy ~args =
  match List.assoc_opt "action" args with
  | Some "add" ->
    Request.categories (fun categories ->
        let form, get_event =
          Admin.add_new_event_form categories in
        let add_button =
          Ui_utils.simple_button
            (fun () -> add_action (get_event ()))
            "Add new event"
        in
        let back = Admin.back_button () in
        set_in_main_page [form; add_button; back];
        finish ()
      )
  | None | Some "edit" ->
    begin
      match List.assoc_opt "id" args with
      | None ->
        Request.events ~args
          (fun events ->
             set_in_main_page
               (Admin.events_list args
                  ~export_action:(fun () ->
                      Request.events ~args (fun events ->
                          Request.title ~args (fun title ->
                              let sep = "%2C" in
                              let title =
                                match title with
                                | None -> sep
                                | Some title -> Data_encoding.title_to_csv ~sep title in
                              let header = Data_encoding.header ~sep in
                              let events =
                                List.fold_left
                                  (fun acc event ->
                                     acc ^ Data_encoding.event_to_csv ~sep event ^ ";%0A")
                                  ""
                                  (snd @@ List.split events) in
                              let str =  (title ^ ";%0A" ^ header ^ ";%0A" ^ events) in
                              Ui_utils.download "database.csv" str; finish ())))
                  ~logout_action
                  events); finish ())
      | Some i -> begin
          try
            let i = int_of_string i in
            Request.categories (fun categories ->
                Request.event ~args i (fun old_event ->
                    let form, get_event =
                      Admin.event_form
                        old_event
                        i
                        categories
                    in
                    let edit_button =
                      Ui_utils.simple_button
                        (fun () -> update_action Admin.compare args i old_event categories (get_event ()))
                        "Update event"
                    in
                    let remove_button =
                      Ui_utils.simple_button
                        (fun () -> remove_action args i)
                        "Remove event"
                    in
                    let back = Admin.back_button () in
                    set_in_main_page [form; edit_button; remove_button; back];
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
      ~login_action
      ~register_action
  ];
  finish ()

let admin_page ~args =
  Request.is_auth (fun logged ->
      Js_utils.log "Logged ? %b" logged;
      if logged then
        admin_page_if_trustworthy ~args
      else
        admin_page_if_not_trustworthy ()
    )

let () =
  add_page ""              main_page;
  add_page Home.page_name  main_page;
  add_page Admin.page_name admin_page
