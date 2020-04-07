open Lwt
open EzAPI.TYPES

module StringMap = StringCompat.StringMap

module Reader = Reader.Reader_generic (Monad_lwt)

let event (_, id) () = Reader.event id >>= EzAPIServerUtils.return

let events _ () = Reader.events () >>= EzAPIServerUtils.return

let add_event _ event = Writer.add_event event; EzAPIServerUtils.return true

let update_event _ (id, event) = Writer.update_event id event |> EzAPIServerUtils.return

let categories _ () = Reader.categories () >>= EzAPIServerUtils.return

let timeline_data req () =
  let start_date =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "start_date" req.req_params in
  let end_date =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "end_date"   req.req_params in
  let group =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "group"      req.req_params in
  let min_ponderation =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "min_level"  req.req_params in
  let max_ponderation =
    Utils.fopt Utils.hd_opt @@ StringMap.find_opt "max_level"  req.req_params in

  let start_date = Utils.fopt Utils.string_to_date start_date in
  let end_date = Utils.fopt Utils.string_to_date end_date in
  let min_ponderation = Utils.fopt int_of_string_opt min_ponderation in
  let max_ponderation = Utils.fopt int_of_string_opt max_ponderation in
  Reader.timeline_data
    ?start_date
    ?end_date
    ?group
    ?min_ponderation
    ?max_ponderation
    () >>= EzAPIServerUtils.return

let remove_event (_, id) () = Writer.remove_event id |> EzAPIServerUtils.return

let reinitialize _ events =
  Writer.remove_events ();
  List.iter Writer.add_event events;
  EzAPIServerUtils.return true
