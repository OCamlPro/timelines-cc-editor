open Timeline_data

let () =
  let input_file =
    try Sys.argv.(1) with
      _ -> Format.printf "You must provide a data file"; exit 1 in
  let json = Data_encoding.file_to_json input_file in
  Data_encoding.write_json json Utils.full_data
