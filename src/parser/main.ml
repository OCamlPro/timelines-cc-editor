open Data_types

let () =
  let input_file =
    try Sys.argv.(1) with
      _ -> Format.printf "You must provide a data file"; exit 1 in
  let option =
    try Some (Sys.argv.(2)) with
      _ -> None in
  match option with
  | Some "--db" ->
    let {title; events} = Data_encoding.file_to_events input_file in
    let () = Writer.add_title title in
    let () =
      List.iter
        Writer.add_event
        events
    in
    exit 0
  | Some "--json" -> begin
      let open Db_intf.Default_monad in
      Reader.title () >>= function
      | None -> Format.printf "No title registered, ending"; exit 1;
      | Some title ->
        Reader.events () >>= fun events ->
        let json =
          Json_encoding.construct Data_encoding.timeline_encoding Data_types.{title; events} in
        Data_encoding.write_json json Utils.full_data
    end
  | _ ->
    begin
      let json = Data_encoding.file_to_json input_file in
      Data_encoding.write_json json Utils.full_data
    end
