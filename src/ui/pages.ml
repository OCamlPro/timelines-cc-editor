open Js_utils
open Js_of_ocaml_tyxml.Tyxml_js.Html

type dispatcher = args:(string * string) list -> unit

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

let error_404 ~path ~args =
  div ~a:[a_class ["center"; "big-block"]] [
    h2 [txt "Error 404"];
    br ();
    txt @@ Format.sprintf "It seems you are lost. I don't know page %s" path;
    br ();
    default_page ~classes:["center"] ()
  ]

let dispatch ~path ~args =
  (*Misc_js.UpdateOnFocus.incr_page ();*)
  try
    match Hashtbl.find pages path with
    | exception Not_found -> set_in_main_page [error_404 ~path ~args]
    | f -> f ~args
  with exn ->
    Js_utils.log "Exception in dispatch of %s: %s"
      path
      (Printexc.to_string exn);
    raise exn

let finish () = Lwt.return (Ok ())
let main_page ~args =
  Request.events
    (fun json ->
       let json = Format.sprintf "{\"events\":%s}" json in
       let cmd =
         Format.asprintf "window.timeline = new TL.Timeline('timeline-embed',%s)" json in
       let () = Js_of_ocaml.Js.Unsafe.js_expr cmd in
       finish ()
    ) |> ignore

let admin_page ~args =
  let content =
  match List.assoc_opt "action" args with
    | None | Some "add" -> [Admin.add_new_event_form ()]
    | Some "edit" ->
      begin
        failwith "TODO"
      end
    | Some _ ->
      [error_404 ~path:"admin" ~args]
  in
  set_in_main_page content

let () =
  add_page ""      main_page;
  add_page "admin" admin_page
