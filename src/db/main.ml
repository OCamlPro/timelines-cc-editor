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
    let () =
      match title with
      | None -> ()
      | Some title -> ignore @@ Writer.add_title title in
    let () =
      List.iter
        Writer.add_event
        (List.rev events)
    in
    exit 0
  | Some "--to-json" -> begin
      let open Db_intf.Default_monad in
      Reader.title () >>= function
      | None -> Format.printf "No title registered, ending"; exit 1;
      | Some title ->
        Reader.events true >>= fun events ->
        let events = List.map snd events in
        let json =
          Json_encoding.construct
            Data_encoding.timeline_encoding Data_types.{title = Some title; events} in
        Data_encoding.write_json json (file ^ ".json")
    end
  | Some "--to-csv" -> begin
      let open Db_intf.Default_monad in
      Reader.title () >>= function
      | None -> Format.printf "No title registered, ending"; exit 1;
      | Some title ->
        Reader.events true >>= fun events ->
        Reader.title () >>= fun title ->
        let events = List.map snd events in
        let sep = "\t" in
        let title =
          match title with
          | None -> sep
          | Some title -> Data_encoding.title_to_csv ~sep title
        in
        let header = Data_encoding.header ~sep in
        let events =
          List.fold_left
            (fun acc event -> acc ^ Data_encoding.event_to_csv ~sep event^"\n")
            ""
            events in
        let chan = open_out (file ^ ".data") in
        output_string chan (title ^ "\n" ^ header ^ "\n" ^ events);
        close_out chan
    end
  | Some s -> failwith "Error: unknown option s"
  | None -> failwith "Error: expection option --db or --json"
