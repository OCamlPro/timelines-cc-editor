open Lwt

module H = ApiHandlers
module S = ApiServices

let start () =
  let dir =
    EzAPIServerUtils.empty
    |> EzAPIServerUtils.register S.event  H.event
    |> EzAPIServerUtils.register S.events H.events
    |> EzAPIServerUtils.register S.add_event H.add_event
    |> EzAPIServerUtils.register S.update_event H.update_event
    |> EzAPIServerUtils.register S.categories H.categories
    |> EzAPIServerUtils.register S.timeline_data H.timeline_data
    |> EzAPIServerUtils.register S.remove_event H.remove_event
  in
  let servers = [ Config.api_port, EzAPIServerUtils.API dir ] in
  Lwt_main.run (
    Printexc.record_backtrace true;
    Printf.eprintf "Starting RPC servers on ports %s\n%!"
      (String.concat ","
         (List.map (fun (port,_) ->
              string_of_int port) servers));
    EzAPIServer.server servers
  )
