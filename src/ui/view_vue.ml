let init title events =
  Js_utils.hide (Js_utils.find_component "page_content-loading");
  Timeline_display.display_timeline title events;
  Timeline_display.init_slide_from_url ~whenOnSlide:(fun _ -> ()) title events
