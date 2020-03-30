
let () =
  let input_file =
    try Sys.argv.(1) with
      _ -> Format.printf "You must provide a data file"; exit 1
  in
  let json = To_json.file_to_json input_file in
  To_json.write_json json Utils.full_data
