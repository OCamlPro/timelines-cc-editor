open Data_types
let () =
  let file =
    try Sys.argv.(1) with
      _ -> Format.printf "You must provide a data file"; exit 1 in
  let option =
    try Some (Sys.argv.(2)) with
      _ -> None in
  match option with
  | Some "--db" ->
    let {title; events} = Data_encoding.file_to_events file in
    let () = Writer.remove_events () in
    let () = ignore @@ Writer.add_title title in
    let () =
      List.iter
        Writer.add_event
        (List.rev events)
    in
    exit 0
  | Some "--json" -> begin
      let open Db_intf.Default_monad in
      Reader.title () >>= function
      | None -> Format.printf "No title registered, ending"; exit 1;
      | Some title ->
        Reader.events true >>= fun events ->
        let events = List.map snd events in
        let json =
          Json_encoding.construct Data_encoding.timeline_encoding Data_types.{title; events} in
        Data_encoding.write_json json (file ^ ".json")
    end
  | Some s -> failwith "Error: unknown option s"
  | None -> failwith "Error: expection option --db or --json"
