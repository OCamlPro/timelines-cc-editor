(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

module Manip = Ezjs_tyxml.Manip

module type Input = sig
  val ids : string list
end

module Make(I : Input) = struct
  let elts =
    List.map
      (fun id ->
        Ezjs_tyxml.log "Preparing %s" id;
        (id, lazy (Ezjs_tyxml.find_component id), lazy (Ezjs_tyxml.find_component (id ^ "-content")))
      )
      I.ids

  let select nav_id =
     List.iter
       (fun (id, elt, content) ->
         if id = nav_id then
           let () = Manip.addClass (Lazy.force elt) "active" in
           Ezjs_tyxml.show (Lazy.force content)
         else
           let () = Manip.removeClass (Lazy.force elt) "active" in
           Ezjs_tyxml.hide (Lazy.force content))
      elts

  let init () =
    List.iter
      (fun (nav_id, elt, _content) ->
        Ezjs_tyxml.log "Adding event listener to %s" nav_id;
        Manip.Ev.onclick (Lazy.force elt) (fun _ -> select nav_id; true)
     )
    elts
end
