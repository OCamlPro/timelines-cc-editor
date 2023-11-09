open Ui_common

let () =
  Lang.init (fun () ->
      let path = Ui_utils.get_path () in
      ignore @@ Pages.dispatch ~path ~args:(Ezjs_loc.args ())
    )
