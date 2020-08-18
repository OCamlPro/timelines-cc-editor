let init token name title events =
  Ui_common.Ui_utils.update_page_title name;
  let () =
    match token with
    | None -> ()
    | Some t -> Timeline_cookies.add_timeline name t true in
  Ui_common.Ui_utils.slow_hide (Js_utils.find_component "page_content-loading");
  (* Todo: parametrize the removal of categories *)
  let remove_category (i, e) = (i, {e with Timeline_data.Data_types.group = None}) in
  let title = Timeline_data.Utils.opt remove_category title in
  let events = List.map remove_category events in
  
  Timeline_display.display_timeline ~view:true title events;
  Timeline_display.init_slide_from_url ~whenOnSlide:(fun _ -> ()) title events
