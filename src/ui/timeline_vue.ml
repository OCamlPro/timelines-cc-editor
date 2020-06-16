open Data_types
open Ui_utils

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
  method adminButton  : Js.js_string Js.t Js.readonly_prop

  method categoryHeader      : Js.js_string Js.t Js.readonly_prop
  method otherFiltersHeader  : Js.js_string Js.t Js.readonly_prop
  method panelHeader         : Js.js_string Js.t Js.readonly_prop
  method minPonderationLabel : Js.js_string Js.t Js.readonly_prop
  method maxPonderationLabel : Js.js_string Js.t Js.readonly_prop
  method filterButtonText : Js.js_string Js.t Js.readonly_prop

  method ponderationHelp : Js.js_string Js.t Js.readonly_prop
  method addElementHelp  : Js.js_string Js.t Js.readonly_prop
  method editElementHelp : Js.js_string Js.t Js.readonly_prop
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
  method backButton            : Js.js_string Js.t Js.readonly_prop
  method removeButton          : Js.js_string Js.t Js.readonly_prop
  method formNameAdding        : Js.js_string Js.t Js.readonly_prop
  method formNameEditing       : Js.js_string Js.t Js.readonly_prop

  method addEventButtonText     : Js.js_string Js.t Js.readonly_prop
  method updateEventButtonText  : Js.js_string Js.t Js.readonly_prop

  (* Values *)

  method minPonderation : int Js.prop
  method maxPonderation : int Js.prop

  method categories : categoryFilter Js.t Js.js_array Js.t Js.prop

  method startDateFormValue    : Js.js_string Js.t Js.prop
  method endDateFormValue      : Js.js_string Js.t Js.prop
  method mediaFormValue        : Js.js_string Js.t Js.prop
  method headlineFormValue     : Js.js_string Js.t Js.prop
  method uniqueIdFormValue     : Js.js_string Js.t Js.prop
  method categoriesFormValue   : Js.js_string Js.t Js.prop
  method textFormValue         : Js.js_string Js.t Js.prop
  method tagsFormValue         : Js.js_string Js.t Js.prop
  method ponderationFormValue  : int Js.prop
  method confidentialFormValue : bool Js.t Js.prop

  method addingNewEvent : bool Js.t Js.prop
  (* Is the form here to add (true) or edit (false) an event *)

  method currentEvent          : Js.js_string Js.t Js.prop
  (* Unique Id of the current event *)

  method currentEventInForm    : Js.js_string Js.t Js.prop
  (* Database Id of the event in the form (when an event is being edited) *)

  method currentTimeline : Js.js_string Js.t Js.readonly_prop
  (* Name of the current timeline *)

  method events : jsEvent Js.t Js.js_array Js.t Js.readonly_prop
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
    val exportButton      = jss "Share timeline"
    val adminButton       = jss "Administration panel"

    val categoryHeader      = jss "Categories"
    val otherFiltersHeader  = jss "Extra filters"
    val panelHeader         = jss "Events"
    val minPonderationLabel = jss "Minimal Ponderation"
    val maxPonderationLabel = jss "Max Ponderation"
    val filterButtonText    = jss "Filter"

    val ponderationHelp     = jss "Select two values"
    val editElementHelp     = jss "Edit the displayed event"
    val addElementHelp      = jss "Add a new event to the timeline"
    val filterHelp          = jss "Apply the filters on the timeline events (not working yet)"

    val startDateFormTitle    = jss "From"
    val endDateFormTitle      = jss "To"
    val mediaFormTitle        = jss "Media"
    val headlineFormTitle     = jss "Headline"
    val uniqueIdFormTitle     = jss "Unique"
    val categoriesFormTitle   = jss "Category"
    val textFormTitle         = jss "Description"
    val tagsFormTitle         = jss "Tags (separate with ',')"
    val ponderationFormTitle  = jss "Ponderation"
    val confidentialFormTitle = jss "Confidential"
    val backButton            = jss "Back"
    val removeButton          = jss "Remove event"

    val formNameAdding    = jss "Add a new event"
    val formNameEditing   = jss "Edit the displayed event"
    val addEventButtonText = jss "Add new event";
    val updateEventButtonText = jss "Update event";
    
    val mutable minPonderation = 0
    val mutable maxPonderation = 100
    val mutable categories     = categories

    val mutable startDateFormValue    = jss ""
    val mutable endDateFormValue      = jss ""
    val mutable mediaFormValue        = jss ""
    val mutable headlineFormValue     = jss ""
    val mutable uniqueIdFormValue     = jss ""
    val mutable categoriesFormValue   = jss ""
    val mutable textFormValue         = jss ""
    val mutable tagsFormValue         = jss ""
    val mutable ponderationFormValue  = 0
    val mutable confidentialFormValue = Js.bool false

    val mutable addingNewEvent = Js.bool false

    val mutable currentEvent = jss ""
    val mutable currentEventInForm = jss ""

    val currentTimeline = jss timeline_name

    val events =
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

let category_component () =
  let template =
    "<div>\n\
     <input \n\
     type='checkbox' \n\
     :id=category.catId\n\
     :value=category.catName \n\
     v-model=category.checked>\n\
     <label :for=category.catId>{{category.catName}}</label>\n\
     </div>" in
  let props = ["category"; "checkedCategories"] in
  Vue_js.component "cat" ~template ~props

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
    | None -> ()
    | Some d ->
      self##.startDateFormValue :=
        jss @@ Format.asprintf "%a" (CalendarLib.Printer.Date.fprint "%Y-%m-%d") d in

  let () = (* end_date *)
    match e.end_date with
    | None -> ()
    | Some d ->
      self##.endDateFormValue :=
        jss @@ Format.asprintf "%a" (CalendarLib.Printer.Date.fprint "%Y-%m-%d") d in

  let () = (* media *)
    match e.media with
    | None -> ()
    | Some {url} -> self##.mediaFormValue := jss url in

  let () = (* group *)
    match e.group with
    | None -> ()
    | Some g -> self##.categoriesFormValue := jss g in

  let () = (* text *)
    self##.headlineFormValue := jss e.text.headline;
    self##.textFormValue := jss e.text.text;
    self##.ponderationFormValue := e.ponderation;
    self##.uniqueIdFormValue := jss e.unique_id;
    self##.confidentialFormValue := Js.bool e.confidential
  in
  ()


(* Methods of the view *)
let showForm title events (self : 'a) (adding : bool) : unit =
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
        let _, e = List.find (fun (_, {unique_id; _}) -> unique_id = current_event_id) events in
        Utils.event_to_metaevent e in
    updateVueFromEvent self current_event
  end;
  ()

let hideForm self =
  Js_utils.Manip.removeClass (Js_utils.find_component "navPanel") "visible";
  self##.addingNewEvent := (Js.bool false)

let addEvent title events self adding : unit =
  let timeline = Js.to_string self##.currentTimeline in
  if timeline = "" then
    Js_utils.alert "Select a timeline before editing it."
  else begin
    let start_date  = Utils.string_to_date @@ Js.to_string self##.startDateFormValue in
    let end_date    = Utils.string_to_date @@ Js.to_string self##.endDateFormValue   in
    let media        = Js.to_string self##.mediaFormValue      in
    let headline     = Js.to_string self##.headlineFormValue   in
    let text         = Js.to_string self##.textFormValue       in
    let unique_id    = Js.to_string self##.uniqueIdFormValue   in
    let group        = Js.to_string self##.categoriesFormValue in
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
        match List.find_opt (fun (_, e) -> e.unique_id = u_id) events with
        | None -> begin
            match title with
            | Some (_, {unique_id; _}) when unique_id = u_id -> title
            | _ ->
              let err = Format.sprintf "Event with id %s cannot be edited!" u_id in 
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
            ~tags
            ~timeline in
        ()
  end

let removeEvent title events self =
  let u_id = Js.to_string self##.currentEventInForm in
  let event_id =
    match List.find_opt (fun (_, e) -> e.unique_id = u_id) events with
    | None -> begin
        match title with
        | Some (i, {unique_id; _}) when unique_id = u_id ->
          Js_utils.alert "You cannot delete the title of your timeline";
          None
        | _ ->
          let err = Format.sprintf "Event with id %s cannot be deleted!" u_id in 
          Js_utils.alert err;
          None
      end
    | Some (i, _e) -> Some i in
  match event_id with
  | None -> ()
  | Some id ->
    let _l : _ Lwt.t = Controller.removeEvent id in
    ()

(* Timeline initializer *)
let display_timeline self title events =
  Timeline_display.display_timeline title events;

  let whenOnSlide = function
    | None ->
      Js_utils.log "Error during slide change, assuming not changed"
    | Some s ->
      self##.currentEvent := jss s;
      Js_utils.log "Current event is %s" s in
  Timeline_display.init_slide_from_url ~whenOnSlide title events

let first_connexion vue =
  Js_utils.alert 
    "Your timeline has been created ! \
     You are now ready to create your first event.";
  Ui_utils.click (Js_utils.find_component "add-event-span")

let init
    ~(on_page: on_page)
    ~(categories : (string * bool) list) =

  (* First : displaying titles *)

  let name,events, title =
    match on_page with
    | Timeline {name; events; title} -> name, events, title
    | No_timeline -> "", [], None in
  let data_js = page_vue name categories events in
  Vue.add_method1 "showForm" (showForm title events);
  Vue.add_method0 "hideForm" hideForm;
  Vue.add_method1 "addEvent" (addEvent title events);
  Vue.add_method0 "removeEvent" (removeEvent title events);

  let _cat = category_component () in
  let vue = Vue.init ~data_js () in

  (* Now displaying timeline *)

  let () =
    match on_page with
    | No_timeline -> Js_utils.alert "No timeline has been selected"
    | Timeline {title; events; name} ->
      match events with
      | [] -> first_connexion vue
      | _ -> display_timeline vue title events 
  in ()
