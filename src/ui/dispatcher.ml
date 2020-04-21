let dispatch : (
  path : string ->
  args : (string * string) list ->
  (unit, unit Xhr_lwt.error) result Lwt.t) ref =
  ref (fun ~path:_ ~args:_ -> assert false)

let main_div_id = "page-content"
let get_main_page () = Js_utils.find_component main_div_id

let set_in_main_page content = Js_utils.Manip.replaceChildren (get_main_page ()) content
