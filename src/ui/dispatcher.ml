let dispatch : (
  ?timeline : string ->
  path : string ->
  args : (string * string) list ->
  unit ->
  (unit, unit Xhr_lwt.error) result Lwt.t) ref =
  ref (fun ?timeline:_ ~path:_ ~args:_ -> assert false)

let validate_dispatch post : unit =
  let open Lwt in
  let _ = (
    post >>= (fun r ->
        let () = match r with
          | Ok () -> Js_utils.log "Dispatch OK"; 
          | Error _ -> Js_utils.log "Dispatch failed"
        in return r            
      )
  ) in ()

let main_div_id = "page-content"
let get_main_page () = Js_utils.find_component main_div_id

let set_in_main_page content = Js_utils.Manip.replaceChildren (get_main_page ()) content

let append_to_end_page content = Js_utils.Manip.appendChild (get_main_page ()) content
