let default_ponderation = 10

let main () =
  Request.timeline_data ~args:["ponderation", string_of_int default_ponderation] (fun events ->
      Request.title ~args:[] (fun title ->
          let events =
            List.map (fun (i,e) ->
              ("id-" ^ string_of_int i),
              {e with Data_types.group = None}
            ) events in
          let cmd =
            let timeline = (events, title) in
            let json = Json_encoding.construct (Data_encoding.id_timeline_encoding) timeline in
            let yoj  = Json_repr.to_yojson json in
            let str  = Yojson.Safe.to_string yoj in
            let () = Js_utils.log "yojson to send: %s" str in
            Format.asprintf
              "window.timeline = new TL.Timeline('timeline-embed',%s)"
              str in
          let () = Js_of_ocaml.Js.Unsafe.js_expr cmd in
          Lwt.return (Ok ())
        )
  )

let () = ignore @@ main ()
