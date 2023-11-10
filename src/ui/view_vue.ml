(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

let init token name title events =
  Ui_common.Ui_utils.update_page_title name;
  let () =
    match token with
    | None -> ()
    | Some t -> Timeline_cookies.add_timeline name t true in
  Ezjs_tyxml.(hide (find_component "alert-div"));
  Ui_common.Ui_utils.slow_hide (Ezjs_tyxml.find_component "page_content-loading");
  Ezjs_tyxml.(show (find_component "page_content"));
  Timeline_display.display_timeline ~view:true title events;
  Timeline_display.init_slide_from_url
    ~whenOnSlide:(fun _ -> ())
    ~activate_keypress:(fun _ -> true)
    title
    events
