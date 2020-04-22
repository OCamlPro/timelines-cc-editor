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
type 'start_date update_meta_event_res =
  | Success
  | Modified of 'start_date Data_types.meta_event option
  | Failed of string

let update_meta_event_res_encoding start_encoding =
  union [
    case
      (option (Data_encoding.meta_event_encoding start_encoding))
      (function Modified e -> Some e | _ -> None) (fun e -> Modified e);
    case
      (tup1 string)
      (function Failed s -> Some s | _ -> None)
      (fun s -> Failed s);
    case (tup1 unit) (function Success -> Some () | _ -> None) (fun () -> Success);
  ]

type update_event_res = CalendarLib.Date.t update_meta_event_res

let update_event_res_encoding = update_meta_event_res_encoding Data_encoding.date_encoding

type update_title_res = CalendarLib.Date.t option update_meta_event_res

let update_title_res_encoding =
  update_meta_event_res_encoding (Json_encoding.option Data_encoding.date_encoding)
