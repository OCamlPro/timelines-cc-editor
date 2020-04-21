let finish () = Lwt.return (Ok ())

(* Controllers *)

let login_action log pwd =
  ignore @@
  Request.login log pwd (function
      | Some auth_data -> begin
          Js_utils.log "Login OK!@.";
          Ui_utils.auth_session log auth_data;
          Js_utils.reload ();
          finish ()
        end
      | None -> begin
          Js_utils.alert "Wrong login/password@.";
          finish ()
        end)

let logout_action args =
  ignore @@
  Request.logout
    ~args
    (fun _ ->
       Ui_utils.logout_session ();
       Js_utils.reload ();
       finish ())

let register_action log pwd =
  ignore @@ Request.register_user log pwd (fun _ -> finish ())

let add_action event =
  Js_utils.log "Adding event %a" Utils.pp_event event;
  let args = Ui_utils.get_args () in
  ignore @@
  Request.add_event ~args
    event
    (function
      | Ok () -> !Dispatcher.dispatch ~path:"" ~args
      | Error s -> Lwt.return (Error (Xhr_lwt.Str_err ("Add new event action failed: " ^ s)))
    )

let remove_action args i =
  let c = Js_utils.confirm "Are you sure you want to remove this event ? This is irreversible." in
  if c then
    ignore @@
    Request.remove_event
      ~args
      i
      (fun _ ->
         ignore @@ !Dispatcher.dispatch ~path:"admin" ~args:[];
         finish ())
  else ()

let rec update_action compare args i old_event categories = (
  fun new_event ->
    Js_utils.log "Update...";
    ignore @@
    Request.update_event ~args i ~old_event ~new_event (
      function
      | Success -> begin
          Js_utils.log "Going back to main page";
          !Dispatcher.dispatch
            ~path:"admin"
            ~args:(["action", "edit"])
        end
      | Failed s -> begin
          Js_utils.log "Update failed: %s" s;
          Lwt.return
            (Error (Xhr_lwt.Str_err ("Update event action failed: " ^ s)))
        end
      | Modified event_opt ->
        Js_utils.log "Event has been modified while editing";
        Dispatcher.set_in_main_page [
          compare
            i
            event_opt
            new_event
            categories
            ~add_action
            ~update_action:(update_action compare args)
            ~remove_action:(remove_action args)
        ];
        finish ()
    )
)
