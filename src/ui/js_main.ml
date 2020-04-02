open Js_utils
open Lwt

let file_url = Utils.raw_data

let () =
  Js_utils.log "Start";
  Request.events
    (fun json ->
       let json = Format.sprintf "{\"events\":%s}" json in
       let cmd =
         Format.asprintf "window.timeline = new TL.Timeline('timeline-embed',%s)" json in
       let () = Js_of_ocaml.Js.Unsafe.js_expr cmd in
       Lwt.return (Ok "Ok")
    )
  |> ignore
(*window.timeline = new TL.Timeline('timeline-embed',json); *)
