open Timeline_data
open Data_types

open Ui_common
open Ui_utils
open Lang
open Text

module Js = Js_of_ocaml.Js

class type jsEvent = object
  method date     : Js.js_string Js.t Js.readonly_prop
  method headline : Js.js_string Js.t Js.readonly_prop
end

class type categoryFilter = object
  method id : int Js.readonly_prop
  method catName : Js.js_string Js.t Js.readonly_prop
  method catId   : Js.js_string Js.t Js.readonly_prop
  method checked : bool Js.t Js.prop
end

class type data = object

  (* Text to display on the page *)
  method exportButton : Js.js_string Js.t Js.readonly_prop

  method categoryHeader      : Js.js_string Js.t Js.readonly_prop
  method otherFiltersHeader  : Js.js_string Js.t Js.readonly_prop
  method panelHeader         : Js.js_string Js.t Js.readonly_prop
  method minPonderationLabel : Js.js_string Js.t Js.readonly_prop
  method maxPonderationLabel : Js.js_string Js.t Js.readonly_prop
  method filterButtonText : Js.js_string Js.t Js.readonly_prop

  method ponderationHelp : Js.js_string Js.t Js.readonly_prop
  method addElementHelp  : Js.js_string Js.t Js.readonly_prop
  method editElementHelp : Js.js_string Js.t Js.readonly_prop
  method exportHelp : Js.js_string Js.t Js.readonly_prop
  method filterHelp : Js.js_string Js.t Js.readonly_prop

  method startDateFormTitle    : Js.js_string Js.t Js.readonly_prop
  method endDateFormTitle      : Js.js_string Js.t Js.readonly_prop
  method mediaFormTitle        : Js.js_string Js.t Js.readonly_prop
  method headlineFormTitle     : Js.js_string Js.t Js.readonly_prop
  method uniqueIdFormTitle     : Js.js_string Js.t Js.readonly_prop
  method categoriesFormTitle   : Js.js_string Js.t Js.readonly_prop
  method textFormTitle         : Js.js_string Js.t Js.readonly_prop
  method tagsFormTitle         : Js.js_string Js.t Js.readonly_prop
  method ponderationFormTitle  : Js.js_string Js.t Js.readonly_prop
  method confidentialFormTitle : Js.js_string Js.t Js.readonly_prop
  method newCategory           : Js.js_string Js.t Js.readonly_prop

  method startDateFormHelp     : Js.js_string Js.t Js.readonly_prop
  method endDateFormHelp       : Js.js_string Js.t Js.readonly_prop
  method mediaLink             : Js.js_string Js.t Js.readonly_prop
  method mediaFormHelp         : Js.js_string Js.t Js.readonly_prop
  method headlineFormHelp      : Js.js_string Js.t Js.readonly_prop
  method uniqueIdFormHelp      : Js.js_string Js.t Js.readonly_prop
  method categoriesFormHelp    : Js.js_string Js.t Js.readonly_prop
  method textFormHelp          : Js.js_string Js.t Js.readonly_prop
  method tagsFormHelp          : Js.js_string Js.t Js.readonly_prop
  method ponderationFormHelp   : Js.js_string Js.t Js.readonly_prop
  method confidentialFormHelp  : Js.js_string Js.t Js.readonly_prop

  method backButton            : Js.js_string Js.t Js.readonly_prop
  method removeButton          : Js.js_string Js.t Js.readonly_prop
  method formNameAdding        : Js.js_string Js.t Js.readonly_prop
  method formNameEditing       : Js.js_string Js.t Js.readonly_prop

  method addEventButtonText    : Js.js_string Js.t Js.readonly_prop
  method updateEventButtonText : Js.js_string Js.t Js.readonly_prop

  (* Values *)

  method minPonderationFilter : int Js.prop
  method maxPonderationFilter : int Js.prop

  method categories : categoryFilter Js.t Js.js_array Js.t Js.prop

  method startDateFormValue     : Js.js_string Js.t Js.prop
  method endDateFormValue       : Js.js_string Js.t Js.prop
  method mediaFormValue         : Js.js_string Js.t Js.prop
  method headlineFormValue      : Js.js_string Js.t Js.prop
  method uniqueIdFormValue      : Js.js_string Js.t Js.prop
  method categoriesFormValue    : Js.js_string Js.t Js.prop
  method textFormValue          : Js.js_string Js.t Js.prop
  method tagsFormValue          : Js.js_string Js.t Js.prop
  method ponderationFormValue   : int Js.prop
  method confidentialFormValue  : bool Js.t Js.prop
  method otherCategoryFormValue : Js.js_string Js.t Js.prop

  method addingNewEvent : bool Js.t Js.prop
  (* Is the form here to add (true) or edit (false) an event *)

  method currentEvent          : Js.js_string Js.t Js.prop
  (* Unique Id of the current event *)

  method currentEventInForm    : Js.js_string Js.t Js.prop
  (* Database Id of the event in the form (when an event is being edited) *)

  method currentTimeline : Js.js_string Js.t Js.readonly_prop
  (* Name of the current timeline *)

  method event_list_ : jsEvent Js.t Js.js_array Js.t Js.readonly_prop
end

module PageContent = struct
  type nonrec data = data
  let id = "page-content"
end

module Vue = Vue_js.Make (PageContent)

let page_vue
    (timeline_name : string)
    (categories : (string * bool) list)
    (event_list : (int * event) list)
  : data Js.t =
  let categories : categoryFilter Js.t Js.js_array Js.t =
    Js.array @@
    Array.of_list @@
    List.mapi
      (fun i (c, checked) ->
         object%js
           val id = i
           val catName = jss c
           val catId = jss (Ui_utils.trim c)
           val mutable checked = Js.bool checked
         end)
      categories
  in object%js
    val exportButton      = tjs_ s_share

    val categoryHeader      = tjs_ s_categories
    val otherFiltersHeader  = tjs_ s_extra_filters
    val panelHeader         = tjs_ s_events
    val minPonderationLabel = tjs_ s_min_ponderation
    val maxPonderationLabel = tjs_ s_max_ponderation
    val filterButtonText    = tjs_ s_filter

    val ponderationHelp     = tjs_ s_ponderation_help
    val editElementHelp     = tjs_ s_edit_element_help
    val addElementHelp      = tjs_ s_add_element_help
    val filterHelp          = tjs_ s_filter_help 
    val exportHelp          = tjs_ s_export_help 

    val startDateFormTitle    = tjs_ s_from
    val endDateFormTitle      = tjs_ s_to
    val mediaFormTitle        = tjs_ s_media
    val headlineFormTitle     = tjs_ s_headline
    val uniqueIdFormTitle     = tjs_ s_unique_id
    val categoriesFormTitle   = tjs_ s_category
    val textFormTitle         = tjs_ s_description
    val tagsFormTitle         = tjs_ s_tags
    val ponderationFormTitle  = tjs_ s_ponderation
    val confidentialFormTitle = tjs_ s_confidential
    val newCategory           = tjs_ s_new_category

    val startDateFormHelp      = tjs_ s_start_date_help
    val endDateFormHelp        = tjs_ s_end_date_help
    val mediaLink              = tjs_ s_media_link
    val mediaFormHelp          = tjs_ s_media_help
    val headlineFormHelp       = tjs_ s_headline_help
    val uniqueIdFormHelp       = tjs_ s_unique_id_help
    val categoriesFormHelp     = tjs_ s_category_help
    val textFormHelp           = tjs_ s_description_help
    val tagsFormHelp           = tjs_ s_tags_help
    val ponderationFormHelp    = tjs_ s_ponderation_help
    val confidentialFormHelp   = tjs_ s_confidential_help

    val backButton            = tjs_ s_back
    val removeButton          = tjs_ s_remove_event

    val formNameAdding        = tjs_ s_add_new_event
    val formNameEditing       = tjs_ s_edit_event
    val addEventButtonText    = tjs_ s_add_new_event
    val updateEventButtonText = tjs_ s_edit_event
    
    val mutable minPonderationFilter = 0
    val mutable maxPonderationFilter = 100
    val mutable categories     = categories

    val mutable startDateFormValue     = jss ""
    val mutable endDateFormValue       = jss ""
    val mutable mediaFormValue         = jss ""
    val mutable headlineFormValue      = jss ""
    val mutable uniqueIdFormValue      = jss ""
    val mutable categoriesFormValue    = jss ""
    val mutable textFormValue          = jss ""
    val mutable tagsFormValue          = jss ""
    val mutable ponderationFormValue   = 0
    val mutable confidentialFormValue  = Js.bool false
    val mutable otherCategoryFormValue = jss ""

    val mutable addingNewEvent = Js.bool false

    val mutable currentEvent = jss ""
    val mutable currentEventInForm = jss ""

    val currentTimeline = jss timeline_name

    val event_list_ =
      Js.array @@ Array.of_list @@
      List.map
        (fun (_, e) ->
           object%js
             val date =
               jss @@  Format.asprintf "%a" (CalendarLib.Printer.Date.fprint "%D") e.start_date
             val headline = jss e.text.headline
           end
        )
        event_list
  end

let category_filter_component data =
  let template =
    "<div class='row uniform'>\n\
     <input \n\
     type='checkbox' \n\
     :id=category.catId\n\
     :value=category.catName \n\
     :checked=category.checked \n\
     v-model=category.checked>\n\
     <label :for=category.catId>{{category.catName}}</label>\n\
     </div>" in
  let props = Vue_component.PrsArray ["category"(*; "checkedCategories"*)] in
  Vue_component.make "cat" ~template ~props ~data

let category_select_component data =
  let template =
    "<div class='row uniform'>\n\
     <input \n\
     type='radio' \n\
     name='category-selection' \n\
     :id=category.catId \n\
     :value=category.catName \n\
     :checked='category.catName == categoriesFormValue' \n\
     v-model=categoriesFormValue \n\
     >\n\
     <label :for=category.catId>{{category.catName}}</label>\n\
     </div>" in
  let props = Vue_component.PrsArray ["category"(*; "checkedCategories"*)] in
  Vue_component.make "cat-select" ~template ~data ~props

type on_page =
  | No_timeline
  | Timeline of {
      name: string;
      title: (int * title) option;
      events: (int * event) list
    }

let updateVueFromEvent self e =
  let () = (* start_date *)
    match e.start_date with
    | None -> self##.startDateFormValue := jss ""
    | Some d ->
      self##.startDateFormValue :=
        jss @@ Format.asprintf "%a" (CalendarLib.Printer.Date.fprint "%Y-%m-%d") d in

  let () = (* end_date *)
    match e.end_date with
    | None -> self##.endDateFormValue := jss ""
    | Some d ->
      self##.endDateFormValue :=
        jss @@ Format.asprintf "%a" (CalendarLib.Printer.Date.fprint "%Y-%m-%d") d in

  let () = (* media *)
    match e.media with
    | None -> self##.mediaFormValue := jss ""
    | Some {url} -> self##.mediaFormValue := jss url in

  let () = (* group *)
    match e.group with
    | None -> self##.categoriesFormValue := jss ""
    | Some g ->
      self##.categoriesFormValue := jss g in

  let () = (* text *)
    self##.headlineFormValue := jss e.text.headline;
    self##.textFormValue := jss e.text.text;
    self##.ponderationFormValue := e.ponderation;
    self##.uniqueIdFormValue := jss e.unique_id;
    self##.confidentialFormValue := Js.bool e.confidential
  in
  ()


(* Methods of the view *)
let showForm title event_list (self : 'a) (adding : bool) : unit =
  Js_utils.Manip.addClass (Js_utils.find_component "navPanel") "visible";
  self##.addingNewEvent := (Js.bool adding);
  if adding then begin
    updateVueFromEvent self {
    start_date = Some (CalendarLib.Date.today ());
    end_date = None;
    text = {text = ""; headline = ""};
    media = None;
    group = None;
    confidential = false;
    ponderation = 0;
    unique_id = "";
    last_update = None;
    tags = []
  }
  end else begin
    self##.currentEventInForm   := self##.currentEvent;
    let current_event =
      let current_event_id = Js.to_string self##.currentEvent in
      match title with
      | Some (_, title) when title.unique_id = current_event_id -> title
      | _ ->
        let _, e = List.find (fun (_, {unique_id; _}) -> unique_id = current_event_id) event_list in
        Utils.event_to_metaevent e in
    updateVueFromEvent self current_event
  end;
  ()

let hideForm self =
  Js_utils.Manip.removeClass (Js_utils.find_component "navPanel") "visible";
  self##.addingNewEvent := (Js.bool false)

let addEvent title event_list self adding : unit =
  let timeline = Js.to_string self##.currentTimeline in
  if timeline = "" then
    Js_utils.alert @@ Lang.t_ Text.s_alert_no_timeline_selected
  else begin
    let start_date  = Utils.string_to_date @@ Js.to_string self##.startDateFormValue in
    let end_date    = Utils.string_to_date @@ Js.to_string self##.endDateFormValue   in
    let media        = Js.to_string self##.mediaFormValue      in
    let headline     = Js.to_string self##.headlineFormValue   in
    let text         = Js.to_string self##.textFormValue       in
    let unique_id    = Js.to_string self##.uniqueIdFormValue   in
    let group        =
      let s = Js.to_string self##.categoriesFormValue in
      if s = "__other__category__" then
        Js.to_string self##.otherCategoryFormValue
      else s
    in
    let tags         = Js.to_string self##.tagsFormValue       in
    let ponderation  = self##.ponderationFormValue             in
    let confidential = Js.to_bool self##.confidentialFormValue in
    if adding then begin
      Js_utils.log "Adding event";
      let _l : 'a Lwt.t =
        Controller.add_event
          ~start_date
          ~end_date
          ~media
          ~headline
          ~text
          ~unique_id
          ~group
          ~ponderation
          ~confidential
          ~tags
          ~timeline
      in ()
    end
    else
      let u_id = Js.to_string self##.currentEventInForm in
      let old_event =
        match List.find_opt (fun (_, e) -> e.unique_id = u_id) event_list with
        | None -> begin
            match title with
            | Some (_, {unique_id; _}) when unique_id = u_id -> title
            | _ ->
              let err = Format.sprintf "%s --> %s" u_id (Lang.t_ Text.s_alert_unknown_event) in 
              Js_utils.alert err;
              None
          end
        | Some (i, e) -> Some (i, Utils.event_to_metaevent e) in
      match old_event with
      | None -> ()
      | Some (id, old_event) ->
        let _l : _ Lwt.t =
          Controller.update_event
            ~id
            ~old_event
            ~start_date
            ~end_date
            ~media
            ~headline
            ~text
            ~unique_id
            ~group
            ~ponderation
            ~confidential
            ~tags in
        ()
  end

let removeEvent title event_list self =
  let u_id = Js.to_string self##.currentEventInForm in
  let event_id =
    match List.find_opt (fun (_, e) -> e.unique_id = u_id) event_list with
    | None -> begin
        match title with
        | Some (_i, {unique_id; _}) when unique_id = u_id ->
          Js_utils.alert @@ Lang.t_ Text.s_alert_title_deletion;
          None
        | _ ->
          let err = Format.sprintf "%s --> %s" u_id (Lang.t_ Text.s_alert_unknown_event) in 
          Js_utils.alert err;
          None
      end
    | Some (i, _e) -> Some i in
  match event_id with
  | None -> ()
  | Some id ->
    let _l : _ Lwt.t = Controller.removeEvent id in
    ()

let export title event_list _ = Controller.export_timeline title event_list  

(* Timeline initializer *)
let display_timeline self title event_list =
  Timeline_display.display_timeline title event_list;
  let whenOnSlide = function
    | None ->
      Js_utils.log "Error during slide change, assuming not changed"
    | Some s ->
      self##.currentEvent := jss s;
      Js_utils.log "Current event is %s" s in
  Timeline_display.init_slide_from_url ~whenOnSlide title event_list

let filter self =
  try
    let args =
      match Args.get_timeline (Args.get_args ()) with
      | None -> failwith "Error: no timeline!"
      | Some t -> Args.set_timeline t [] in  
    let args = (* Filtering categories *)
      let categories_js = Js.to_array self##.categories in
      Array.fold_left
        (fun acc cat ->
           if Js.to_bool cat##.checked then
             Args.add_category (Js.to_string cat##.catName) acc
           else acc
        )
        args
        categories_js in
    let args = (* Filtering by ponderation *)
      let min_ponderation = self##.minPonderationFilter in
      let max_ponderation = self##.maxPonderationFilter in
      Args.(set_min min_ponderation @@ set_max max_ponderation args)
    in
    Ui_utils.(push (url "timeline" args));
    ignore @@ !Dispatcher.dispatch ~path:"timeline" ~args
  with Failure s -> Js_utils.alert s

let first_connexion _vue =
  Js_utils.alert @@ Lang.t_ Text.s_alert_timeline_creation;
  Ui_utils.click (Js_utils.find_component "add-event-span")

let init
    ~(on_page: on_page)
    ~(categories : (string * bool) list) =

  Js_utils.log "Creating vue@.";
  let name,event_list, title =
    match on_page with
    | Timeline {name; events; title} -> name, events, title
    | No_timeline -> "", [], None in
  let data_js = page_vue name categories event_list in
  Js_utils.log "Adding methods@.";
  Vue.add_method1 "showForm" (showForm title event_list);
  Vue.add_method0 "hideForm" hideForm;
  Vue.add_method1 "addEvent" (addEvent title event_list);
  Vue.add_method0 "removeEvent" (removeEvent title event_list);
  Vue.add_method0 "exportTimeline" (export title event_list);
  Vue.add_method0 "filter" filter;

  Js_utils.log "Adding components@.";
  let _cat = category_filter_component (fun _ -> data_js) in
  let _cat = category_select_component (fun _ -> data_js) in
  Js_utils.log "Initializing vue@.";
  let vue = Vue.init ~data_js () in
  Js_utils.log "Displaying timeline@.";

  let () =
    match on_page with
    | No_timeline -> Js_utils.alert "No timeline has been selected"
    | Timeline {title; events; name=_} ->
      match events with
      | [] -> first_connexion vue
      | _ -> display_timeline vue title events 
  in ()
