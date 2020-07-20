open Js_utils

module type Input = sig
  val ids : string list
end

module Make(I : Input) = struct
  
  let elts = 
    List.map
      (fun id ->
        Js_utils.log "Preparing %s" id;
        (id, lazy (Js_utils.find_component id), lazy (Js_utils.find_component (id ^ "-content")))
      )
      I.ids

  let select nav_id =
     List.iter
       (fun (id, elt, content) ->
         if id = nav_id then
           let () = Manip.addClass (Lazy.force elt) "active" in
           Js_utils.show (Lazy.force content)
         else
           let () = Manip.removeClass (Lazy.force elt) "active" in
           Js_utils.hide (Lazy.force content))
      elts

  let init () =
    List.iter
      (fun (nav_id, elt, _content) -> 
        Js_utils.log "Adding event listener to %s" nav_id;
        Manip.Ev.onclick (Lazy.force elt) (fun _ -> select nav_id; true)
     )
    elts
end
