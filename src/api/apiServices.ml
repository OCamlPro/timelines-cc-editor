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
(*
let update_event : (int * Data_types.event, int) post_service0 =
  post_service
    ~name:"update_events"
    ~input:(Json_encoding.tup2 EzEncoding.tup1_int Data_encoding.event_encoding)
    ~output:tup1_int
    Path.(root // "update_events") *)

(*
let address : (int, Address.t option) service1 =
  service
    ~section:section_spot
    ~name:"address"
    ~output:(Json_encoding.option AddressEncoding.encoding)
    Path.(root // "address" /: arg_address_key)

let party : (int, Party.t option) service1 =
  service
    ~section:section_spot
    ~name:"party"
    ~output:(Json_encoding.option PartyEncoding.encoding)
    Path.(root // "party" /: arg_party_key)

let party_by_name : (string, Party.t list) service1 =
  service
    ~section:section_spot
    ~name:"party_by_name"
    ~output:(Json_encoding.list PartyEncoding.encoding)
    Path.(root // "party_by_name" /: arg_name)

let parties : (Party.t list) service0 =
  service
    ~section:section_spot
    ~name:"parties"
    ~output:(Json_encoding.list PartyEncoding.encoding)
    Path.(root // "parties")

let asset : (int, Asset.t option) service1 =
  service
    ~section:section_spot
    ~name:"asset"
    ~output:(Json_encoding.option AssetEncoding.encoding)
    Path.(root // "asset" /: arg_asset_key)

let po_line : (int, POLine.t option) service1 =
  service
    ~section:section_spot
    ~name:"po_line"
    ~output:(Json_encoding.option POLineEncoding.encoding)
    Path.(root // "po_line"/: arg_po_line_key)

let purchase_order : (int, PurchaseOrder.t option) service1 =
  service
    ~section:section_spot
    ~name:"purchase_order"
    ~output:(Json_encoding.option PurchaseOrderEncoding.encoding)
    Path.(root // "purchase_order"/: arg_purchase_order_key)

let equipment : (int, Equipment.t option) service1 =
  service
    ~section:section_spot
    ~name:"equipment"
    ~output:(Json_encoding.option EquipmentEncoding.encoding)
    Path.(root // "equipment"/: arg_booking_key)

let booking : (int, Booking.t option) service1 =
  service
    ~section:section_spot
    ~name:"booking"
    ~output:(Json_encoding.option BookingEncoding.encoding)
    Path.(root // "booking"/: arg_booking_key)

let shipment : (int, Shipment.t option) service1 =
  service
    ~section:section_spot
    ~name:"ship_transport_details"
    ~output:(Json_encoding.option ShipmentEncoding.encoding)
    Path.(root // "ship_transport_details"/: arg_shipment_key)

let isf_data : (int, ISFData.t option) service1 =
  service
    ~section:section_spot
    ~name:"isf_data"
    ~output:(Json_encoding.option ISFDataEncoding.encoding)
    Path.(root // "isf_data" /: arg_isf_data_key)

let status_messages : (int, StatusMessages.t option) service1 =
  service
    ~section:section_spot
    ~name:"status_messages"
    ~output:(Json_encoding.option StatusMessagesEncoding.encoding)
    Path.(root // "status_messages" /: arg_status_messages_key)

let all_status_messages : StatusMessages.t list service0 =
  service
    ~section:section_spot
    ~name:"all_status_messages"
    ~output:(Json_encoding.list StatusMessagesEncoding.encoding)
    Path.(root // "all_status_messages")

let contract : (int, Contract.t option) service1 =
  service
    ~section:section_spot
    ~name:"contract"
    ~output:(Json_encoding.option ContractEncoding.encoding)
    Path.(root // "contract" /: arg_contract_key)

let contracts : Contract.t list service0 =
  service
    ~section:section_spot
    ~name:"contract"
    ~output:(Json_encoding.list ContractEncoding.encoding)
    Path.(root // "contracts")

let all_purchase_orders : PurchaseOrder.t list service0 =
  service
    ~section:section_spot
    ~name:"all_purchase_orders"
    ~output:(Json_encoding.list PurchaseOrderEncoding.encoding)
    Path.(root // "all_purchase_orders")

let purchase_orders_of_contract : (Contract.key, PurchaseOrder.t list) service1 =
  service
    ~section:section_spot
    ~name:"purchase_orders_of_contract"
    ~output:(Json_encoding.list PurchaseOrderEncoding.encoding)
    Path.(root // "contract_pos" /: arg_contract_key)

let po_lines_of_purchase_order : (PurchaseOrder.key, POLine.t list) service1 =
  service
    ~section:section_spot
    ~name:"po_lines_of_purchase_order"
    ~output:(Json_encoding.list POLineEncoding.encoding)
    Path.(root // "po_lines" /: arg_purchase_order_key)

let all_bookings : Booking.t list service0 =
  service
    ~section:section_spot
    ~name:"all_bookings"
    ~output:(Json_encoding.list BookingEncoding.encoding)
    Path.(root // "all_bookings")

let all_shipments : Shipment.t list service0 =
  service
    ~section:section_spot
    ~name:"all_shipments"
    ~output:(Json_encoding.list ShipmentEncoding.encoding)
    Path.(root // "all_shipments")

let shipments_of_contract : (int, Shipment.t list) service1 =
  service
    ~section:section_spot
    ~name:"shipment_of_contract"
    ~output:(Json_encoding.list ShipmentEncoding.encoding)
    Path.(root // "contract_shipments" /: arg_contract_key)

let isf_of_contract : (int, ISFData.t list) service1 =
  service
    ~section:section_spot
    ~name:"isf_of_contract"
    ~output:(Json_encoding.list ISFDataEncoding.encoding)
    Path.(root // "contract_isfs" /: arg_contract_key)

(*
let register_address : (Address.raw, Address.key) post_service0 =
  post_service
    ~section:section_spot
    ~name:"register_address"
    ~input:AddressEncoding.raw_encoding
    ~output:Json_encoding.(tup1 int)
    Path.(root // "register_address") *)

let register_party : (Party.raw, Party.key) post_service0 =
  post_service
    ~section:section_spot
    ~name:"register_party"
    ~input:PartyEncoding.raw_encoding
    ~output:Json_encoding.(tup1 int)
    Path.(root // "register_party")

let register_asset : (Asset.raw, Asset.key) post_service0 =
  post_service
    ~section:section_spot
    ~name:"register_asset"
    ~input:AssetEncoding.raw_encoding
    ~output:Json_encoding.(tup1 int)
    Path.(root // "register_asset")

let register_po_line :
  (POLine.raw, POLine.key) post_service0 =
  post_service
    ~section:section_spot
    ~name:"register_po_line"
    ~input:POLineEncoding.raw_encoding
    ~output:Json_encoding.(tup1 int)
    Path.(root // "register_po_line")

let register_purchase_order :
  (PurchaseOrder.raw, PurchaseOrder.key) post_service0 =
  post_service
    ~section:section_spot
    ~name:"register_purchase_order"
    ~input:PurchaseOrderEncoding.raw_encoding
    ~output:Json_encoding.(tup1 int)
    Path.(root // "register_purchase_order")

let register_booking :
  (Booking.raw, Booking.key) post_service0 =
  post_service
    ~section:section_spot
    ~name:"register_booking"
    ~input:BookingEncoding.raw_encoding
    ~output:Json_encoding.(tup1 int)
    Path.(root // "register_booking")

let register_shipment :
  (Shipment.raw, Shipment.key) post_service0 =
  post_service
    ~section:section_spot
    ~name:"register_ship_transport_details"
    ~input:ShipmentEncoding.raw_encoding
    ~output:Json_encoding.(tup1 int)
    Path.(root // "register_ship_transport_details")

let register_isf_data :
  (ISFData.raw, ISFData.key) post_service0 =
  post_service
    ~section:section_spot
    ~name:"register_isf_data"
    ~input:ISFDataEncoding.raw_encoding
    ~output:Json_encoding.(tup1 int)
    Path.(root // "register_isf_data")

let register_status_messages :
  (StatusMessages.raw, StatusMessages.key) post_service0 =
  post_service
    ~section:section_spot
    ~name:"register_isf_data"
    ~input:StatusMessagesEncoding.raw_encoding
    ~output:Json_encoding.(tup1 int)
    Path.(root // "register_status_messages")

(*
let update_address : (Address.key, Address.raw, unit) post_service1 =
  post_service
    ~section:section_spot
    ~name:"update_address"
    ~input:AddressEncoding.raw_encoding
    ~output:Json_encoding.unit
    Path.(root // "update_address" /: arg_address_key) *)

let update_party : (Party.key, Party.raw, unit) post_service1 =
  post_service
    ~section:section_spot
    ~name:"update_party"
    ~input:PartyEncoding.raw_encoding
    ~output:Json_encoding.unit
    Path.(root // "update_party" /: arg_party_key)

let update_asset : (Asset.key, Asset.raw, unit) post_service1 =
  post_service
    ~section:section_spot
    ~name:"update_asset"
    ~input:AssetEncoding.raw_encoding
    ~output:Json_encoding.unit
    Path.(root // "update_asset" /: arg_asset_key)

let update_po_line :
  (POLine.key, POLine.raw, unit) post_service1 =
  post_service
    ~section:section_spot
    ~name:"update_po_line"
    ~input:POLineEncoding.raw_encoding
    ~output:Json_encoding.unit
    Path.(root // "update_po_line" /: arg_po_line_key)

let update_purchase_order :
  (PurchaseOrder.key, PurchaseOrder.raw, unit) post_service1 =
  post_service
    ~section:section_spot
    ~name:"update_purchase_order"
    ~input:PurchaseOrderEncoding.raw_encoding
    ~output:Json_encoding.unit
    Path.(root // "update_purchase_order" /: arg_purchase_order_key)

let update_booking :
  (Booking.key, Booking.raw, unit) post_service1 =
  post_service
    ~section:section_spot
    ~name:"update_booking"
    ~input:BookingEncoding.raw_encoding
    ~output:Json_encoding.unit
    Path.(root // "update_booking" /: arg_booking_key)

let update_shipment :
  (Shipment.key, Shipment.raw, unit) post_service1 =
  post_service
    ~section:section_spot
    ~name:"update_ship_transport_details"
    ~input:ShipmentEncoding.raw_encoding
    ~output:Json_encoding.unit
    Path.(root // "update_ship_transport_details" /: arg_shipment_key)

let update_isf_data :
  (ISFData.key, ISFData.raw, unit) post_service1 =
  post_service
    ~section:section_spot
    ~name:"update_isf_data"
    ~input:ISFDataEncoding.raw_encoding
    ~output:Json_encoding.unit
    Path.(root // "update_isf_data" /: arg_isf_data_key)

let update_status_messages :
  (StatusMessages.key, StatusMessages.raw, unit) post_service1 =
  post_service
    ~section:section_spot
    ~name:"update_status_messages"
    ~input:StatusMessagesEncoding.raw_encoding
    ~output:Json_encoding.unit
    Path.(root // "update_status_messages" /: arg_status_messages_key)
(*
let remove_address : (Address.key, unit) post_service0 =
  post_service
    ~section:section_spot
    ~name:"remove_address"
    ~input:Json_encoding.(tup1 int)
    ~output:Json_encoding.unit
    Path.(root // "remove_address") *)

let remove_party : (Party.key, unit) post_service0 =
  post_service
    ~section:section_spot
    ~name:"remove_party"
    ~input:Json_encoding.(tup1 int)
    ~output:Json_encoding.unit
    Path.(root // "remove_party")

let remove_asset : (Asset.key, unit) post_service0 =
  post_service
    ~section:section_spot
    ~name:"remove_asset"
    ~input:Json_encoding.(tup1 int)
    ~output:Json_encoding.unit
    Path.(root // "remove_asset")

let remove_po_line : (POLine.key, unit) post_service0 =
  post_service
    ~section:section_spot
    ~name:"remove_po_line"
    ~input:Json_encoding.(tup1 int)
    ~output:Json_encoding.unit
    Path.(root // "remove_po_line")

let remove_purchase_order : (PurchaseOrder.key, unit) post_service0 =
  post_service
    ~section:section_spot
    ~name:"remove_purchase_order"
    ~input:Json_encoding.(tup1 int)
    ~output:Json_encoding.unit
    Path.(root // "remove_purchase_order")

let remove_shipment : (Shipment.key, unit) post_service0 =
  post_service
    ~section:section_spot
    ~name:"remove_ship_transport_details"
    ~input:Json_encoding.(tup1 int)
    ~output:Json_encoding.unit
    Path.(root // "remove_ship_transport_details")

let remove_isf_data : (ISFData.key, unit) post_service0 =
  post_service
    ~section:section_spot
    ~name:"remove_isf_data"
    ~input:Json_encoding.(tup1 int)
    ~output:Json_encoding.unit
    Path.(root // "remove_isf_data")

let remove_status_messages : (StatusMessages.key, unit) post_service0 =
  post_service
    ~section:section_spot
    ~name:"remove_status_messages"
    ~input:Json_encoding.(tup1 int)
    ~output:Json_encoding.unit
    Path.(root // "remove_status_messages")

let send_order : (POLine.key, unit) post_service0 =
  post_service
    ~section:section_spot
    ~name:"send_order"
    ~input:Json_encoding.(tup1 int)
    ~output:Json_encoding.unit
    Path.(root // "send_order")

let accept_order : (POLine.key, unit) post_service0 =
  post_service
    ~section:section_spot
    ~name:"accept_order"
    ~input:Json_encoding.(tup1 int)
    ~output:Json_encoding.unit
    Path.(root // "accept_order")

let book_order : (POLine.key, unit) post_service0 =
  post_service
    ~section:section_spot
    ~name:"book_order"
    ~input:Json_encoding.(tup1 int)
    ~output:Json_encoding.unit
    Path.(root // "book_order")

let ship_order : (POLine.key, unit) post_service0 =
  post_service
    ~section:section_spot
    ~name:"ship_order"
    ~input:Json_encoding.(tup1 int)
    ~output:Json_encoding.unit
    Path.(root // "ship_order")

let get_sent_orders : ((PurchaseOrder.key * POLine.t list) list) service0 =
  service
    ~section:section_spot
    ~name:"get_sent_orders"
    ~output:Json_encoding.(list (tup2 int (list POLineEncoding.encoding)))
    ~params:[param_number; param_page]
    Path.(root // "get_sent_orders")

let register_csm :
  (StatusMessages.key, StatusMessages.custom_status_message, unit) post_service1 =
  post_service
    ~section:section_spot
    ~name:"register_csm"
    ~input:StatusMessagesEncoding.custom_sm_encoding
    ~output:Json_encoding.unit
    Path.(root // "register_csm" /: arg_status_messages_key)


let test : unit service0  =
  service
    ~section:section_spot
    ~name:"test"
    ~params:[]
    ~output:Json_encoding.empty
    Path.(root // "test")


let partners : (int32 * partner) list service0  =
  service
    ~section:section_spot
    ~name:"partners"
    ~output:(list (tup2 int32 partner_encoding))
    ~params:[param_number; param_page]
    Path.(root // "partners")

let nb_partners : int service0  =
  service
    ~section:section_spot
    ~name:"nb_partners"
    ~output:tup1_int
    ~params:[]
    Path.(root // "partners" // "nb")

let get_partner : (int,partner api_result) service1 =
  service
    ~section:section_spot
    ~name:"get_partner"
    ~output:(api_result_encoding partner_encoding)
    Path.(root // "partners" // "get" /: arg_party_key)

let search_partners : (string,(int32 * partner) list) service1 =
  service
    ~section:section_spot
    ~name:"search_partners"
    ~output:(list (tup2 int32 partner_encoding))
    Path.(root // "partners" // "search" /: arg_name)

let remove_partner : (int32, unit api_result) post_service0 =
  post_service
    ~section:section_spot
    ~name:"register"
    ~input:(tup1 int32)
    ~output:default_result_encoding
    Path.(root // "partners" // "remove")

let register_partner : (partner, int32 api_result) post_service0 =
  post_service
    ~section:section_spot
    ~name:"register"
    ~input:partner_encoding
    ~output:(api_result_encoding int32)
    Path.(root // "partners" // "new")

let update_partner : (int32 * partner, unit api_result) post_service0 =
  post_service
    ~section:section_spot
    ~name:"register"
    ~input:(tup2 int32 partner_encoding)
    ~output:default_result_encoding
    Path.(root // "partners" // "update")

(* session stuff *)

let register_admin : (User.user_auth, default_result) post_service0 =
  post_service
    ~section:section_session
    ~name:"register"
    ~input:UserEncoding.user_auth_encoding
    ~output:default_result_encoding
    Path.(root // "register_admin")

let register_user : (User.user_auth, default_result) post_service0 =
  post_service
    ~section:section_session
    ~name:"register"
    ~input:UserEncoding.user_auth_encoding
    ~output:default_result_encoding
    Path.(root // "register_user")

let change_password : (User.chpwd_info, default_result) post_service0 =
  post_service
    ~section:section_session
    ~name:"change_password"
    ~input:UserEncoding.chpwd_info_encoding
    ~output:default_result_encoding
    Path.(root // "change_password")

let users : User.user_info list service0  =
  service
    ~section:section_session
    ~name:"users"
    ~output:(list SpotAPIEncoding.UserEncoding.user_info_encoding)
    ~params:[param_number; param_page]
    Path.(root // "users")

let nb_users : int service0  =
  service
    ~section:section_session
    ~name:"nb_users"
    ~output:tup1_int
    ~params:[]
    Path.(root // "users" // "nb")


(* ISF Stuff  *)

let submit_isf : (int, ISFData.status * ISFData.trstat) service1 =
  service
    ~section:section_spot
    ~name:"submit_isf"
    ~output:(ISFDataEncoding.status_trstat_encoding)
    Path.(root // "submit_isf" /: arg_isf_data_key)

let get_isf_status : (int, ISFData.status * ISFData.trstat) service1 =
  service
    ~section:section_spot
    ~name:"get_isf_status"
    ~output:(ISFDataEncoding.status_trstat_encoding)
    Path.(root // "get_isf_status" /: arg_isf_data_key)

let get_ship_pos : (string, string * string) service1 =
  service
    ~section:section_spot
    ~name:"ship_pos"
    ~output:Json_encoding.(tup2 string string)
    Path.(root // "ship_pos" /: arg_mmsi)
*)
