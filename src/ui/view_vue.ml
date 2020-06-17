
let init title events =
  Timeline_display.display_timeline title events;
  Timeline_display.init_slide_from_url ~whenOnSlide:(fun _ -> ()) title events
