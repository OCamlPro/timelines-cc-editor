open Json_encoding

(* The default return type of API requests *)
type api_result = (unit, string) result

let api_result_encoding : api_result Json_encoding.encoding =
  Json_encoding.(
    union
      [
        case unit (fun _ -> Some ()) (fun () -> Ok ());
        case string (function Error s -> Some s | _ -> None) (fun s -> Error s)
      ]
  )

(* Updates require a "Modified" case *)
type update_event_res =
  | Success
  | Modified of Data_types.event option
  | Failed of string

let update_event_res_encoding =
  union [
    case
      (option Data_encoding.event_encoding)
      (function Modified e -> Some e | _ -> None) (fun e -> Modified e);
    case
      (tup1 string)
      (function Failed s -> Some s | _ -> None)
      (fun s -> Failed s);
    case (tup1 unit) (function Success -> Some () | _ -> None) (fun () -> Success);
  ]


