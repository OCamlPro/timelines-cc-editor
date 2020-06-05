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

let dispatch ?timeline ~path ~args () =
  try
    match Hashtbl.find pages path with
    | exception Not_found -> set_in_main_page [error_404 ~path ~args ()]; finish ()
    | f ->
      let args =
        match timeline with
        | None -> args
        | Some t -> Ui_utils.add_arg_unique "timeline" t args in
      let url = Ui_utils.url path args in
      Ui_utils.push url;
      Js_utils.log "Pushing %s" url;
      f ~args
  with exn ->
    Js_utils.log "Exception in dispatch of %s: %s"
      path
      (Printexc.to_string exn);
    raise exn

let () = Dispatcher.dispatch := dispatch

let timeline_id args cont =
  match timeline_id_from_args args with
  | None ->
    set_in_main_page [txt "Error: Timeline id not found !"];
    Lwt.return (Error (Xhr_lwt.Str_err "Export database failed: no timeline specified"))
  | Some timeline -> cont timeline
  

let main_page ~args =
  timeline_id args (fun timeline ->
      Request.timeline_data ~args timeline (fun events ->
          Request.is_auth (fun is_auth ->
              Request.title ~args timeline (fun title ->
                  Request.categories timeline (fun categories ->
                      let page, init =
                        Home.page
                          timeline
                          is_auth
                          args
                          categories
                          title
                          events
                      in
                      set_in_main_page [page];
                      init ();
                      finish ()
                    )
                )
            )
        )
    )

let admin_page_if_trustworthy ~timeline ~args =
  Js_utils.log "Admin page: trustworthy version";
  match List.assoc_opt "action" args with
  | Some "add" ->
    Js_utils.log "Adding an element";
    timeline_id args (fun timeline ->
      Request.categories timeline (fun categories ->
          let form, get_event =
            Admin.add_new_event_form categories in
          let add_button =
            Ui_utils.simple_button
              "add-button-trust"
              (fun _ -> Controller.add_action args timeline (get_event ()))
              "Add new event"
          in
          let back = Admin.back_button timeline in
          set_in_main_page [form; add_button; back];
          finish ()
          )
      )
  | None | Some "edit" -> begin
    timeline_id args (fun timeline ->
      Js_utils.log "Edition or main_page";
      match List.assoc_opt "id" args with
      | None ->
        Js_utils.log "No id provided";
        Request.events ~args timeline (fun events ->
          Request.timeline_users timeline (fun users ->
            let users = 
              match users with 
              | Ok u -> u
              | Error _ -> Js_utils.log "Error while requesting users"; [] in
            set_in_main_page [
              Admin.admin_main_page 
                timeline
                args
                events
                users
              ]; finish ()))
      | Some i -> begin
          try
            Js_utils.log "Editing event %s" i;
            let i = int_of_string i in
            Request.categories timeline (fun categories ->
              Request.event ~args i (fun old_event ->
                let form, get_event =
                  Admin.event_form
                    old_event
                    i
                    categories
                in
                let edit_button =
                  Ui_utils.simple_button
                    "edit-button-trust"
                    (fun _ ->
                       update_action
                         (Admin.compare timeline)
                         i
                         categories
                         old_event
                         (get_event ())
                         (fun _ ->
                            !Dispatcher.dispatch
                              ~path:Home.page_name
                              ~timeline
                              ~args:["id", string_of_int i]
                              ())
                    )
                    "Update event"
                in
                let remove_button =
                  Ui_utils.simple_button
                    "remove-button-trust"
                    (fun _ -> remove_action args i)
                    "Remove event"
                in
                let back = Admin.back_button timeline in
                set_in_main_page [form; edit_button; remove_button; back];
                finish ())
              )
          with
            Invalid_argument _ ->
            let msg = Format.sprintf "Invalid event id %s" i in
            set_in_main_page [error_404 ~msg ~path:Admin.page_name ~args ()]; finish ()
        end
        )
  end
  | Some _ -> set_in_main_page [error_404 ~path:Admin.page_name ~args ()]; finish ()

let admin_page_if_not_trustworthy () =
  set_in_main_page [
    Admin.admin_page_login ()
  ];
  finish ()

let admin_page ~args =
  Request.is_auth (fun logged ->
      timeline_id args (fun timeline ->
          Request.has_admin_rights timeline (fun admin ->
              Js_utils.log "Logged ? %b %b" logged admin;
              if logged && admin then
                admin_page_if_trustworthy ~args ~timeline
              else
                admin_page_if_not_trustworthy ()
            )
        )
    )

let select_page ~args =
  Request.is_auth
    (fun logged ->
       if logged then begin
         Request.user_timelines (function
             | Ok l ->
               let content = Timeline_select.timelines_list l in
               Dispatcher.set_in_main_page [content];
               finish ()
             | Error s ->
               alert s;
               finish ()
           )
       end
       else begin
         Dispatcher.set_in_main_page [
           Admin.admin_page_login ~allow_registration:true ()
         ];
         finish ()
       end
    )
    
let view_page ~args =
  timeline_id args (fun timeline ->
    Request.timeline_data ~args timeline (fun events ->
      Request.title ~args timeline (fun title ->
        let () = View.make_page
          events
          title in
        Lwt.return (Ok ())
      )
    )
  )        
  

let () =
  add_page ""              select_page;
  add_page Home.page_name  main_page;
  add_page Admin.page_name admin_page;
  add_page View.page_name  view_page
