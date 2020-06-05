open Json_encoding

(* The default return type of API requests *)
type 'a api_result = ('a, string) result

let api_result_encoding (param : 'a Json_encoding.encoding) : 'a api_result Json_encoding.encoding =
  Json_encoding.(
    union
      [
        (* Let the error case at the beginning. *)
        case 
          (tup2 string string)
          (function | Error s -> Some (s, "__error__") | _ -> None)
          (fun (s, str) ->
             if str = "__error__" then Error s else assert false); 
        case param (function | Ok s -> Some s | _ -> None) (fun s -> Ok s);
      ]
  )

let unit_api_result_encoding : unit api_result Json_encoding.encoding =
  api_result_encoding Json_encoding.unit 

let str_api_result_encoding : string api_result Json_encoding.encoding =
  api_result_encoding Json_encoding.string

let str_list_api_result_encoding : string list api_result Json_encoding.encoding =
  api_result_encoding Json_encoding.(list string)

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
    case unit
      (function Success -> Some () | _ -> None)
      (fun _ -> Success );
  ]

type update_event_res = CalendarLib.Date.t option update_meta_event_res

let update_event_res_encoding =
  update_meta_event_res_encoding (Json_encoding.option Data_encoding.date_encoding)
