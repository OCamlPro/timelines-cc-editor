let dispatch : (
  path: string ->
  args : (string * string) list ->
  (unit, unit Xhr_lwt.error) result Lwt.t) ref = (* Set in pages.ml *)
  ref (fun ~path:_ ~args:_ -> assert false)
