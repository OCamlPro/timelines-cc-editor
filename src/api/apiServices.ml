open Timeline_data
open Json_encoding
open Api_data.ApiData
open EzAPI

let tup1_int = EzEncoding.tup1_int

let arg_default id = Arg.int id ~example:(-1)

let arg_token () = Arg.string "timeline-id" ~example:"timeline-id"

type nonrec 'a service0 = ('a, string, Security.basic) service0
type nonrec ('a, 'b) service1 = ('a, 'b, string, Security.basic) service1
type nonrec ('a, 'b) post_service0 = ('a, 'b, string, Security.basic) post_service0
type nonrec ('a, 'b, 'c) post_service1 = ('a, 'b, 'c, string, Security.basic) post_service1

let api_root =
  match !Config.API.api_root with
  | None -> Path.root
  | Some p -> Path.(root // p)

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

let unit = Api_data.ApiData.unit

let group_param = Param.{
  param_id = "group";
  param_name  = Some "group";
  param_descr = Some "Category";
  param_type = PARAM_STRING;
  param_required = false;
  param_examples = ["OCaml"; "Software"];
  param_schema = None;
}

let email_param = Param.{
  param_id = "email";
  param_name  = Some "email";
  param_descr = Some "Email";
  param_type = PARAM_STRING;
  param_required = false;
  param_examples = ["your@provider.mail"];
  param_schema = None;
}

let lang_param = Param.{
  param_id = "lang";
  param_name  = Some "lang";
  param_descr = Some "Language";
  param_type = PARAM_STRING;
  param_required = false;
  param_examples = ["fr"; "en"];
  param_schema = None;
}

let group_list_param = Param.{
  param_id = "group_list";
  param_name  = Some "group_list";
  param_descr = Some "Categories";
  param_type = PARAM_STRING;
  param_required = false;
  param_examples = ["OCaml"; "Software,OCaml"];
  param_schema = None;
}

let tags_param = Param.{
  param_id = "tags";
  param_name  = Some "tags";
  param_descr = Some "Tags";
  param_type = PARAM_STRING;
  param_required = false;
  param_examples = ["ocaml"; "tag1,tag2"];
  param_schema = None;
}

let date_param name = Param.{
  param_id = "date";
  param_name  = Some name;
  param_descr = Some "Date";
  param_type = PARAM_STRING;
  param_required = false;
  param_examples = ["01-01-1111"; "05-12-1991"];
  param_schema = None;
}

let ponderation_param name = Param.{
  param_id = "ponderation";
  param_name  = Some name;
  param_descr = Some "Ponderation";
  param_type = PARAM_INT;
  param_required = false;
  param_examples = ["0"; "9"];
  param_schema = None;
}

let confid_param = Param.{
  param_id = "confidential";
  param_name  = Some "confidential";
  param_descr = Some "confidential";
  param_type = PARAM_BOOL;
  param_required = false;
  param_examples = ["false"; "true"];
  param_schema = None;
}

let readonly_param = Param.{
  param_id = "readonly";
  param_name  = Some "readonly";
  param_descr = Some "readonly";
  param_type = PARAM_BOOL;
  param_required = false;
  param_examples = ["false"; "true"];
  param_schema = None;
}

let event_id = Param.{
  param_id = "event_id";
  param_name  = Some "event_id";
  param_descr = Some "EventId";
  param_type = PARAM_INT;
  param_required = true;
  param_examples = ["0"; "9"; "42"];
  param_schema = None;
}

let pretty_name_param = Param.{
  param_id = "pretty";
  param_name  = Some "pretty";
  param_descr = Some "Pretty name";
  param_type = PARAM_STRING;
  param_required = false;
  param_examples = ["Me"; "Your best friend"];
  param_schema = None;
}

let auth_params = Param.{
  param_id = "auth_email";
  param_name  = Some "auth_email";
  param_descr = Some "Email";
  param_type = PARAM_STRING;
  param_required = false;
  param_examples = ["my-email@provider.com"];
  param_schema = None;
} :: {
  param_id = "auth_data";
  param_name  = Some "auth_data";
  param_descr = Some "Key";
  param_type = PARAM_STRING;
  param_required = false;
  param_examples = [];
  param_schema = None;
} :: []

let filter_params = [
  date_param "after";
  date_param "before";
  group_param;
  ponderation_param "min_level";
  ponderation_param "max_level";
  confid_param;
  tags_param
]

let param_number =
  Param.int ~name:"page_size" ~descr:"Number of replies" "n"
let param_page =
  Param.int ~name:"page" ~descr:"Offset in number of pages" "p"

(*
let event : (int, Data_types.title) service1 =
  service
    ~params:auth_params
    ~name:"event"
    ~output:(Data_encoding.title_encoding)
    Path.(api_root // "event" /: (arg_default "event_key"))

let events : (string, (int * Data_types.event) list) service1 =
  service
    ~params:auth_params
    ~name:"events"
    ~output:(list (tup2 int Data_encoding.event_encoding))
    Path.(api_root // "events" /: arg_token ())
let title : (string, (int * Data_types.title) option) service1 =
  service
    ~params:auth_params
    ~name:"title"
    ~output:title_api_result_encoding
    Path.(api_root // "title" /: arg_token ()) *)

let api_error_output =
  EzAPI.Err.Case {
    code = 500;
    name = "Api Error";
    encoding = (Json_encoding.(obj1 (req "error" string)));
    select = (fun s -> Some s);
    deselect = Fun.id
  }

let errors = [
  api_error_output
]

let add_event : (string, Data_types.event, string) post_service1 =
  post_service
    ~errors
    ~params:auth_params
    ~name:"add_event"
    ~input:Data_encoding.event_encoding
    ~output:string
    Path.(api_root // "add_event" /: arg_token ())

let update_event : (int * Data_types.title * Data_types.title * string, update_title_res) post_service0 =
  post_service
    ~errors
    ~params:auth_params
    ~name:"update_event"
    ~input:(tup4 tup1_int Data_encoding.title_encoding Data_encoding.title_encoding string)
    ~output:update_title_res_encoding
    Path.(api_root // "update_event")

let timeline_data :
  (string, DbData.timeline_data_output) service1 =
  service
    ~errors
    ~name:"timeline_data"
    ~output:timeline_data_api_result_encoding
    ~params:(auth_params @ filter_params)
    Path.(api_root // "timeline_data" /: arg_token ())

let remove_event : (string, unit) service1 =
  service
    ~errors
    ~params:(event_id :: auth_params)
    ~name:"remove_event"
    ~output:unit
    Path.(api_root // "remove_event" /: arg_token ())

let categories : (string, string list) service1 =
  service
    ~errors
    ~params:auth_params
    ~name:"categories"
    ~output:(list string)
    Path.(api_root // "categories" /: arg_token ())

let register_user : (string * string, unit) post_service0 =
  post_service
    ~errors
    ~name:"register_user"
    ~input:(tup2 string string)
    ~output:unit
    Path.(api_root // "register_user")

let login : (string * string, string) post_service0 =
  post_service
    ~errors
    ~name:"login"
    ~input:(tup2 string string)
    ~output:string
    Path.(api_root // "login")

let logout : (string * string, unit) post_service0 =
  post_service
    ~errors
    ~name:"login"
    ~input:(tup2 string string)
    ~output:unit
    Path.(api_root // "logout")


let is_auth : (unit, bool) post_service0 =
  post_service
    ~errors
    ~name:"is_auth"
    ~input:unit
    ~output:bool
    Path.(api_root // "is_auth")

let has_admin_rights : (string, unit, bool) post_service1 =
  post_service
    ~errors
    ~params:auth_params
    ~name:"has_admin_rights"
    ~input:unit
    ~output:bool
    Path.(api_root // "has_admin_rights" /: arg_token ())

let export_database : (string, unit) service1 =
  service
    ~errors
    ~params:auth_params
    ~name:"export_database"
    ~output:unit
    Path.(api_root // "export_database" /: arg_token ())

let create_timeline : (string, (Data_types.title * bool), string * string) post_service1 =
  post_service
    ~errors
    ~params:(email_param :: lang_param :: auth_params)
    ~name:"create_timeline"
    ~input:(tup2 Data_encoding.title_encoding bool)
    ~output:Api_data.ApiData.create_timeline_output_encoding
    Path.(api_root // "create_timeline" /: arg_token ())

let import_timeline : (string, (Data_types.title * Data_types.event list * bool), unit) post_service1 =
  post_service
    ~errors
    ~params:auth_params
    ~name:"import_timeline"
    ~input:(tup3 Data_encoding.title_encoding (list Data_encoding.event_encoding) bool)
    ~output:unit
    Path.(api_root // "import_timeline" /: arg_token ())

let user_timelines : (unit, string list) post_service0 =
  post_service
    ~errors
    ~params:auth_params
    ~name:"user_timeline"
    ~input:unit
    ~output:(list string)
    Path.(api_root // "user_timelines")

let allow_user : (string * string, unit) post_service0 =
  post_service
    ~errors
    ~params:auth_params
    ~name:"allow_user"
    ~input:(tup2 string string)
    ~output:unit
    Path.(api_root // "allow_user")

let timeline_users : (string, unit, string list) post_service1 =
  post_service
    ~errors
    ~params:auth_params
    ~name:"timeline_user"
    ~input:unit
    ~output:(list string)
    Path.(api_root // "timeline_users"/: arg_token ())

let remove_user : (unit, unit) post_service0 =
  post_service
    ~errors
    ~params:auth_params
    ~name:"remove_user"
    ~input:unit
    ~output:unit
    Path.(api_root // "remove_user")

let remove_timeline : (string, unit, unit) post_service1 =
  post_service
    ~errors
    ~params:auth_params
    ~name:"remove_timeline"
    ~input:unit
    ~output:unit
    Path.(api_root // "remove_timeline"/: arg_token ())

let create_token : (string, unit, string) post_service1 =
  post_service
    ~errors
    ~params:(auth_params @ filter_params @ [
        readonly_param;
        pretty_name_param
      ])
    ~name:"create_token"
    ~input:unit
    ~output:string
    Path.(api_root // "create_token" /: arg_token ())

let update_token_pretty : (string, string, unit) post_service1 =
  post_service
    ~errors
    ~params:(auth_params @ [
        pretty_name_param
      ])
    ~name:"update_token_pretty"
    ~input:admin_token (* The admin token *)
    ~output:unit
    Path.(api_root // "update_token_pretty" /: arg_token ())

let update_token_readonly : (string, string, unit) post_service1 =
  post_service
    ~errors
    ~params:(auth_params @ [
        readonly_param;
      ])
    ~name:"update_token_readonly"
    ~input:admin_token (* The admin token *)
    ~output:unit
    Path.(api_root // "update_token_readonly" /: arg_token ())

let update_token : (string, string, unit) post_service1 =
  post_service
    ~errors
    ~params:(auth_params @ filter_params @ [
        readonly_param;
        pretty_name_param
      ])
    ~name:"update_token"
    ~input:admin_token (* The admin token *)
    ~output:unit
    Path.(api_root // "update_token" /: arg_token ())

let remove_token : (string, string, unit) post_service1 =
  post_service
    ~errors
    ~params:[]
    ~name:"remove_token"
    ~input:admin_token (* The admin token *)
    ~output:unit
    Path.(api_root // "remove_token" /: arg_token ())

let get_tokens : (string, unit, DbData.filter list) post_service1 =
  post_service
    ~errors
    ~params:[]
    ~name:"get_tokens"
    ~input:unit
    ~output:(list Api_data.ApiData.filter_encoding)
    Path.(api_root // "get_tokens" /: arg_token ())

let timeline_name : (string, string) service1 =
  service
    ~errors
    ~name:"timeline_name"
    ~output:string
    Path.(api_root // "timeline_name" /: arg_token ())

let update_timeline_name : (string, unit) post_service0 =
  post_service
    ~errors
    ~params:(auth_params @ [
        pretty_name_param
      ])
    ~name:"update_timeline_name"
    ~input:any_token
    ~output:unit
    Path.(api_root // "update_timeline_name")

(* Miscelaneous *)
let version : string service0 =
  service
    ~errors
    ~name:"version"
    ~output:string
    Path.(api_root // "version")
