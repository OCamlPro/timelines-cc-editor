let init token name title events =
  Ui_common.Ui_utils.update_page_title name;
  let () =
    match token with
    | None -> ()
    | Some t -> Timeline_cookies.add_timeline name t true in
  Js_utils.hide (Js_utils.find_component "alert-div");
  Ui_common.Ui_utils.slow_hide (Js_utils.find_component "page_content-loading");
  Js_utils.show (Js_utils.find_component "page_content");
  Timeline_display.display_timeline ~view:true title events;
  Timeline_display.init_slide_from_url
    ~whenOnSlide:(fun _ -> ())
    ~activate_keypress:(fun _ -> true)
    title
    events
