open Js_utils
open Lwt

let file_url = Utils.raw_data

let () =
  Js_utils.log "Start";
  Xhr_lwt.get file_url >>=
  (function
    | Ok file ->
      Js_utils.log "File %s" file;
      let json =
        try
          let log_error line error = Js_utils.log "Error at line %s: %s@." line error in
          let timeline = To_json.str_to_events ~log_error file in
          Json_encoding.construct To_json.timeline_encoding timeline
        with Failure s -> Js_utils.log "Error while on line %s@." s; exit 1
      in
      let yojson = Json_repr.to_yojson json in
      let cmd =
        Format.asprintf "window.timeline = new TL.Timeline('timeline-embed',%a)"
          (Json_repr.pp (module Json_repr.Yojson)) yojson in
      let () = Js_of_ocaml.Js.Unsafe.js_expr cmd in
      Lwt.return (Ok "Ok")
    | Error _ -> failwith "Error getting file"
  ) |> ignore
(*window.timeline = new TL.Timeline('timeline-embed',json); *)
