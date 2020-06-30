open Json_encoding

let title_api_result_encoding : ((int * Timeline_data.Data_types.title) option) Json_encoding.encoding =
  option (tup2 int Data_encoding.title_encoding) 

let timeline_data_api_result_encoding : (((int * Timeline_data.Data_types.title) option) * ((int * Timeline_data.Data_types.event) list)) Json_encoding.encoding =
  tup2 (
    option @@
    tup2
      int
      Data_encoding.title_encoding
  ) (
    list @@
    tup2
      int
      Data_encoding.event_encoding
  )

(* Updates require a "Modified" case *)
type 'start_date update_meta_event_res =
  | Success
  | Modified of 'start_date Timeline_data.Data_types.meta_event option

let update_meta_event_res_encoding start_encoding =
  union [
    case
      (option (Data_encoding.meta_event_encoding start_encoding))
      (function Modified e -> Some e | _ -> None) (fun e -> Modified e);
    case unit
      (function Success -> Some () | _ -> None)
      (fun _ -> Success );
  ]

type update_event_res = CalendarLib.Date.t update_meta_event_res
type update_title_res = CalendarLib.Date.t option update_meta_event_res

let update_event_res_encoding =
  update_meta_event_res_encoding Data_encoding.date_encoding
let update_title_res_encoding =
  update_meta_event_res_encoding (Data_encoding.(option date_encoding))
