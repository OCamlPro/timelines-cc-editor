open Data_encoding
open Lwt

let read_json file cont =
  Xhr_lwt.get file >>=
  (function
    | Ok yojson ->
      let yojson = Yojson.Safe.from_string yojson in
      let ezjsonm = Json_repr.from_yojson yojson in
      cont @@ Json_encoding.destruct timeline_encoding ezjsonm
    | Error e -> failwith "Error getting json file"
  )
