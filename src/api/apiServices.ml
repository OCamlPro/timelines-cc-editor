open EzAPI
open Data_types
open Data_encoding

let tup1_int = EzEncoding.tup1_int

let arg_default id = arg_int id (-1)

(*
let arg_address_key = arg_default "address_key"

let arg_asset_key = arg_default "asset_key"

let arg_party_key = arg_default "party_key"

let arg_po_line_key = arg_default "po_line_key"

let arg_equipment_key = arg_default "equipment_key"

let arg_booking_key = arg_default "booking_key"

let arg_purchase_order_key = arg_default "purchase_order_key"

let arg_shipment_key = arg_default "ship_transport_details_key"

let arg_isf_data_key = arg_default "isf_data_key"

let arg_status_messages_key = arg_default "status_messages_key"

let arg_contract_key = arg_default "contract_key"

let arg_mmsi = arg_string "mmsi" ""

let arg_name = arg_string "name" "" *)

let group_param = {
  param_value = "group";
  param_name  = Some "group";
  param_descr = Some "Category";
  param_type = PARAM_STRING;
  param_required = false;
  param_examples = ["OCaml"; "Software"]
}

let date_param name = {
  param_value = "date";
  param_name  = Some name;
  param_descr = Some "Date";
  param_type = PARAM_STRING;
  param_required = false;
  param_examples = ["01-01-1111"; "05-12-1991"]
}

let ponderation_param name = {
  param_value = "ponderation";
  param_name  = Some name;
  param_descr = Some "Ponderation";
  param_type = PARAM_INT;
  param_required = false;
  param_examples = ["0"; "9"]
}

let confidential_param = {
  param_value = "confidential";
  param_name  = Some "confidential";
  param_descr = Some "Confidential";
  param_type = PARAM_STRING;
  param_required = false;
  param_examples = ["true"; "false"]
}

let param_number =
  Param.int ~name:"page_size" ~descr:"Number of replies" "n"
let param_page =
  Param.int ~name:"page" ~descr:"Offset in number of pages" "p"

let event : (int, Data_types.event option) service1 =
  service
    ~name:"event"
    ~output:(Json_encoding.option Data_encoding.event_encoding)
    Path.(root // "event" /: (arg_default "event_key"))

let events : ((int * Data_types.event) list) service0 =
  service
    ~name:"events"
    ~output:(Json_encoding.(list (tup2 int Data_encoding.event_encoding)))
    Path.(root // "events")

let add_event : (Data_types.event, api_result) post_service0 =
  post_service
    ~name:"add_event"
    ~input:Data_encoding.event_encoding
    ~output:api_result_encoding
    Path.(root // "add_event")

let update_event : (int * Data_types.event, api_result) post_service0 =
  post_service
    ~name:"update_event"
    ~input:(Json_encoding.tup2 tup1_int Data_encoding.event_encoding)
    ~output:api_result_encoding
    Path.(root // "update_event")

let categories : (string list) service0 =
  service
    ~name:"categories"
    ~output:(Json_encoding.(list string))
    Path.(root // "categories")

let timeline_data : ((int * Data_types.event) list) service0 =
  service
    ~name:"timeline_data"
    ~output:(Json_encoding.(list (tup2 int Data_encoding.event_encoding)))
    ~params:[
      date_param "start_date";
      date_param "end_date";
      group_param;
      ponderation_param "min_level";
      ponderation_param "max_level";
      confidential_param;
    ]
    Path.(root // "timeline_data")

let remove_event : (int, api_result) service1 =
  service
    ~name:"remove_event"
    ~output:api_result_encoding
    Path.(root // "remove_event" /: (arg_default "event_key"))

let register_user : (string * string, api_result) post_service0 =
  post_service
    ~name:"register_user"
    ~input:(Json_encoding.(tup2 string string))
    ~output:api_result_encoding
    Path.(root // "register_user")

let login : (string * string, bool) post_service0 =
  post_service
    ~name:"login"
    ~input:(Json_encoding.(tup2 string string))
    ~output:Json_encoding.bool
    Path.(root // "login")
