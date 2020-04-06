open Lwt

module Reader = Reader.Reader_generic (Monad_lwt)

let event (_, id) () = Reader.event id >>= EzAPIServerUtils.return

let events _ () = Reader.events () >>= EzAPIServerUtils.return

let add_event _ event = Writer.add_event event; EzAPIServerUtils.return true

let update_event _ (id, event) = Writer.update_event id event |> EzAPIServerUtils.return

let categories _ () = Reader.categories () >>= EzAPIServerUtils.return

let timeline_data _ () = Reader.timeline_data () >>= EzAPIServerUtils.return

let remove_event (_, id) () = Writer.remove_event id |> EzAPIServerUtils.return 
