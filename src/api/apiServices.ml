open EzAPI

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

let param_number =
  Param.int ~name:"page_size" ~descr:"Number of replies" "n"
let param_page =
  Param.int ~name:"page" ~descr:"Offset in number of pages" "p"

let event : (int, Data_types.event option) service1 =
  service
    ~name:"event"
    ~output:(Json_encoding.option Data_encoding.event_encoding)
    Path.(root // "event" /: (arg_default "event_key"))

let events : (Data_types.event list) service0 =
  service
    ~name:"events"
    ~output:(Json_encoding.list Data_encoding.event_encoding)
    Path.(root // "events")

let add_event : (Data_types.event, bool) post_service0 =
  post_service
    ~name:"add_event"
    ~input:Data_encoding.event_encoding
    ~output:Json_encoding.bool
    Path.(root // "add_event")

let update_event : (int * Data_types.event, bool) post_service0 =
  post_service
    ~name:"update_event"
    ~input:(Json_encoding.tup2 tup1_int Data_encoding.event_encoding)
    ~output:Json_encoding.bool
    Path.(root // "update_event")

let categories : (string list) service0 =
  service
    ~name:"categories"
    ~output:(Json_encoding.(list string))
    Path.(root // "categories")
