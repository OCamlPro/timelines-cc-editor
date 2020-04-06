let dispatch : (
  path : string ->
  args : (string * string) list ->
  (unit, unit Xhr_lwt.error) result Lwt.t) ref =
  ref (fun ~path:_ ~args:_ -> assert false)
