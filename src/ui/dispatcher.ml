let dispatch : (
  path: string ->
  args : (string * string) list ->
  (unit, string) result Lwt.t) ref = (* Set in pages.ml *)
  ref (fun ~path:_ ~args:_ -> assert false)
