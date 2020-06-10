open EzAPI
open Data_types
open Data_encoding
open ApiData
open Json_encoding

let tup1_int = EzEncoding.tup1_int

let arg_default id = arg_int id (-1)

let arg_timeline () = arg_string "timeline-id" "timeline-id"

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

let tags_param = {
  param_value = "tags";
  param_name  = Some "tags";
  param_descr = Some "Tags";
  param_type = PARAM_STRING;
  param_required = false;
  param_examples = ["ocaml"; "tag1,tag2"]
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

let auth_params = {
  param_value = "auth_email";
  param_name  = Some "auth_email";
  param_descr = Some "Email";
  param_type = PARAM_STRING;
  param_required = false;
  param_examples = ["my-email@provider.com"]
} :: {
  param_value = "auth_data";
  param_name  = Some "auth_data";
  param_descr = Some "Key";
  param_type = PARAM_STRING;
  param_required = false;
  param_examples = []
} :: []

let param_number =
  Param.int ~name:"page_size" ~descr:"Number of replies" "n"
let param_page =
  Param.int ~name:"page" ~descr:"Offset in number of pages" "p"

let event : (int, Data_types.title option) service1 =
  service
    ~params:auth_params
    ~name:"event"
    ~output:(option Data_encoding.title_encoding)
    Path.(root // "event" /: (arg_default "event_key"))

let events : (string, (int * Data_types.event) list) service1 =
  service
    ~params:auth_params
    ~name:"events"
    ~output:(list (tup2 int Data_encoding.event_encoding))
    Path.(root // "events" /: arg_timeline ())

let title : (string, (int * Data_types.title) api_result) service1 =
  service
    ~params:auth_params
    ~name:"title"
    ~output:(title_api_result_encoding)
    Path.(root // "title" /: arg_timeline ())

let add_event : (string, Data_types.event, string api_result) post_service1 =
  post_service
    ~params:auth_params
    ~name:"add_event"
    ~input:Data_encoding.event_encoding
    ~output:str_api_result_encoding
    Path.(root // "add_event" /: arg_timeline ())

let update_event : (int * Data_types.title * Data_types.title, update_event_res) post_service0 =
  post_service
    ~params:auth_params
    ~name:"update_event"
    ~input:(tup3 tup1_int Data_encoding.title_encoding Data_encoding.title_encoding)
    ~output:update_event_res_encoding
    Path.(root // "update_event")

let timeline_data :
  (string,
   (((int * Data_types.title) option) * ((int * Data_types.event) list)) api_result) service1 =
  service
    ~name:"timeline_data"
    ~output:ApiData.timeline_data_api_result_encoding
    ~params:(auth_params @ [
      date_param "start_date";
      date_param "end_date";
      group_param;
      ponderation_param "min_level";
      ponderation_param "max_level";
      tags_param;
    ])
    Path.(root // "timeline_data" /: arg_timeline ())

let remove_event : (int, unit api_result) service1 =
  service
    ~params:auth_params
    ~name:"remove_event"
    ~output:unit_api_result_encoding
    Path.(root // "remove_event" /: (arg_default "event_key"))

let categories : (string, string list) service1 =
  service
    ~params:auth_params
    ~name:"categories"
    ~output:(list string)
    Path.(root // "categories" /: arg_timeline ())

let register_user : (string * string, unit api_result) post_service0 =
  post_service
    ~name:"register_user"
    ~input:(tup2 string string)
    ~output:unit_api_result_encoding
    Path.(root // "register_user")

let login : (string * string, string option) post_service0 =
  post_service
    ~name:"login"
    ~input:(tup2 string string)
    ~output:(option string)
    Path.(root // "login")

let logout : (string * string, unit) post_service0 =
  post_service
    ~name:"login"
    ~input:(tup2 string string)
    ~output:unit
    Path.(root // "logout")


let is_auth : (unit, bool) post_service0 =
  post_service
    ~name:"is_auth"
    ~input:unit
    ~output:bool
    Path.(root // "is_auth")

let has_admin_rights : (string, unit, bool) post_service1 =
  post_service
    ~params:auth_params
    ~name:"has_admin_rights"
    ~input:unit
    ~output:bool
    Path.(root // "has_admin_rights" /: arg_timeline ())

let export_database : (string, unit api_result) service1 =
  service
    ~params:auth_params
    ~name:"export_database"
    ~output:ApiData.unit_api_result_encoding
    Path.(root // "export_database" /: arg_timeline ())

let create_timeline : (string, (title * bool), string api_result) post_service1 =
  post_service
    ~params:auth_params
    ~name:"create_timeline"
    ~input:(tup2 Data_encoding.title_encoding bool)
    ~output:ApiData.str_api_result_encoding
    Path.(root // "create_timeline" /: arg_timeline ())

let user_timelines : (unit, string list api_result) post_service0 =
  post_service
    ~params:auth_params
    ~name:"create_timeline"
    ~input:unit
    ~output:ApiData.str_list_api_result_encoding
    Path.(root // "user_timelines")

let allow_user : (string * string, unit api_result) post_service0 =
  post_service
    ~params:auth_params
    ~name:"allow_user"
    ~input:(tup2 string string)
    ~output:ApiData.unit_api_result_encoding
    Path.(root // "allow_user")

let timeline_users : (string, unit, string list api_result) post_service1 =
  post_service
    ~params:auth_params
    ~name:"timeline_user"
    ~input:unit
    ~output:ApiData.str_list_api_result_encoding
    Path.(root // "timeline_users"/: arg_timeline ())

let remove_user : (unit, unit api_result) post_service0 =
  post_service
    ~params:auth_params
    ~name:"remove_user"
    ~input:unit
    ~output:ApiData.unit_api_result_encoding
    Path.(root // "remove_user")

let remove_timeline : (string, unit, unit api_result) post_service1 =
  post_service
    ~params:auth_params
    ~name:"remove_timeline"
    ~input:unit
    ~output:ApiData.unit_api_result_encoding
    Path.(root // "remove_timeline"/: arg_timeline ())

let get_view_token : (string, string api_result) service1 =
  service
    ~params:auth_params
    ~name:"get_view_token"
    ~output:ApiData.str_api_result_encoding
    Path.(root // "get_view_token" /: arg_timeline ())
