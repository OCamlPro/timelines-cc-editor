open Lwt

module Reader = Reader.Reader_generic (Monad_lwt)

let event (_, id) () = Reader.event id >>= EzAPIServerUtils.return

let events _ () = Reader.events () >>= EzAPIServerUtils.return

let add_event _ event = Writer.add_event event; EzAPIServerUtils.return true
