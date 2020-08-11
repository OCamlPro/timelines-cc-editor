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

class type filter = object
  method id : int Js.readonly_prop
  method filter_id_ : Js.js_string Js.t Js.readonly_prop
  method pretty : Js.js_string Js.t Js.prop
  method readonly : bool Js.t Js.prop
  method after : Js.js_string Js.t Js.optdef Js.prop
  method before : Js.js_string Js.t Js.optdef Js.prop
  method min_level_ : Js.js_string Js.t Js.optdef Js.prop
  method max_level_ : Js.js_string Js.t Js.optdef Js.prop
  method filterCategories : Js.js_string Js.t Js.js_array Js.t Js.optdef Js.prop
  method tags : Js.js_string Js.t Js.js_array Js.t Js.optdef Js.prop
  method confidential_rights_ : bool Js.t Js.prop

  method isEditing : bool Js.t Js.prop
  method editing : Js.js_string Js.t Js.optdef Js.prop

end

let filter_to_jsfilter =
  let i = ref 0 in fun DbData.{timeline; kind; pretty; after;
          before; min_level; max_level;
          categories; tags; confidential_rights} : filter Js.t ->
  let readonly =
    match kind with
    | View -> true
    | Edit -> false in

  let pretty =
     match pretty with
     | None -> timeline
     | Some p -> p in

  let after =
     match after with
     | None -> Js.Optdef.empty
     | Some d ->
       Js.Optdef.return @@ jss @@ Format.asprintf "%a" (CalendarLib.Printer.Date.fprint "%D") d in

  let before =
     match before with
     | None -> Js.Optdef.empty
     | Some d ->
       Js.Optdef.return @@ jss @@ Format.asprintf "%a" (CalendarLib.Printer.Date.fprint "%D") d in

  let min_level =
    match min_level with
    | None -> Js.Optdef.empty
    | Some m -> Js.Optdef.return @@ jss @@ Int32.to_string m in

  let max_level =
    match max_level with
    | None -> Js.Optdef.empty
    | Some m -> Js.Optdef.return @@ jss @@ Int32.to_string m in

  let categories =
    match categories with
    | None -> Js.Optdef.empty
    | Some l -> Js.Optdef.return @@ Ui_utils.list_to_jsarray @@ List.map jss l in

  let tags =
    match tags with
    | None -> Js.Optdef.empty
    | Some l -> Js.Optdef.return @@ Ui_utils.list_to_jsarray @@ List.map jss l in
  let obj =
    object%js
      val id = !i
      val filter_id_ = jss timeline
      val mutable pretty = jss pretty
      val mutable readonly = Js.bool readonly
      val mutable after = after
      val mutable before = before
      val mutable min_level_ = min_level
      val mutable max_level_ = max_level
      val mutable filterCategories = categories
      val mutable tags = tags
      val mutable confidential_rights_ = Js.bool confidential_rights

      val mutable isEditing = Js.bool false
      val mutable editing = Js.Optdef.empty
    end
  in i := !i + 1;
  obj


class type data = object

  (* Text to display on the page *)
  method exportButton : Js.js_string Js.t Js.readonly_prop

  method categoryHeader      : Js.js_string Js.t Js.readonly_prop
  method otherFiltersHeader  : Js.js_string Js.t Js.readonly_prop
  method panelHeader         : Js.js_string Js.t Js.readonly_prop
  method minPonderationLabel : Js.js_string Js.t Js.readonly_prop
  method maxPonderationLabel : Js.js_string Js.t Js.readonly_prop
  method filterButtonText    : Js.js_string Js.t Js.readonly_prop
  method categoriesText      : Js.js_string Js.t Js.readonly_prop
  method ponderationText     : Js.js_string Js.t Js.readonly_prop
  method andText             : Js.js_string Js.t Js.readonly_prop
  method betweenText         : Js.js_string Js.t Js.readonly_prop
  method addEditionTokenButton : Js.js_string Js.t Js.readonly_prop
  method addReadonlyTokenButton : Js.js_string Js.t Js.readonly_prop

  method ponderationHelp : Js.js_string Js.t Js.readonly_prop
  method addElementHelp  : Js.js_string Js.t Js.readonly_prop
  method editElementHelp : Js.js_string Js.t Js.readonly_prop
  method exportHelp : Js.js_string Js.t Js.readonly_prop
  method importHelp : Js.js_string Js.t Js.readonly_prop
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

  method confidentialFilterForm : Js.js_string Js.t Js.readonly_prop
  
  method backButton            : Js.js_string Js.t Js.readonly_prop
  method removeButton          : Js.js_string Js.t Js.readonly_prop
  method formNameAdding        : Js.js_string Js.t Js.readonly_prop
  method formNameEditing       : Js.js_string Js.t Js.readonly_prop

  method addEventButtonText    : Js.js_string Js.t Js.readonly_prop
  method updateEventButtonText : Js.js_string Js.t Js.readonly_prop
  method removeTimelineText : Js.js_string Js.t Js.readonly_prop

  method editionLinkText       : Js.js_string Js.t Js.readonly_prop
  method readLinkText          : Js.js_string Js.t Js.readonly_prop

  method timelineListText      : Js.js_string Js.t Js.readonly_prop
  (* Values *)

  (* Filters *)
  method minPonderationFilter : int Js.prop
  method maxPonderationFilter : int Js.prop
  method maxBoundInPonderationFilter : int Js.readonly_prop
  method categories : categoryFilter Js.t Js.js_array Js.t Js.prop
  method alsoConfidential : bool Js.t Js.prop


  method timelineName : Js.js_string Js.t Js.prop
  method newTimelineName : Js.js_string Js.t Js.prop

  method openedMenu : bool Js.t Js.prop


  method startDateFormValue     : Js.js_string Js.t Js.prop
  method endDateFormValue       : Js.js_string Js.t Js.prop
  method mediaFormValue         : Js.js_string Js.t Js.prop
  method headlineFormValue      : Js.js_string Js.t Js.prop
  method uniqueIdFormValue      : Js.js_string Js.t Js.prop
  method uniqueIdFormValueDefault : Js.js_string Js.t Js.prop
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

  method events_ : jsEvent Js.t Js.js_array Js.t Js.readonly_prop
  (* Current events *)

  method filters : filter Js.t Js.js_array Js.t Js.prop
  (* Tokens *)
      
  method cookieTimelines : Timeline_cookies.urlData Js.t Js.js_array Js.t Js.readonly_prop
  (* Timelines in cookies *)
end

module PageContent = struct
  type nonrec data = data
  type all = data
  let id = "page_content"
end

module Vue = Vue_js.Make (PageContent)

module FilterNavs = Navs.Make (
  struct
    let ids = [
      "filter-categories";
      "filter-ponderation";
      "filter-other"
   ]
  end)

let page_vue
    (timeline_id : string)
    (timeline_name : string)
    (args : Args.t)
    (categories : (string * bool) list)
    (title : (int * title) option)
    (events : (int * event) list)
    (tokens : DbData.filter list)
  : data Js.t =
  let categories : categoryFilter Js.t Js.js_array Js.t =
    Ui_utils.list_to_jsarray @@
    List.mapi
      (fun i (c, checked) ->
         object%js
           val id = i
           val catName = jss c
           val catId = jss (Ui_utils.trim c)
           val mutable checked = Js.bool checked
         end)
      categories in

  let max = match Args.get_max args with
    | None -> 
      let title_pond = 
        match title with 
        | None -> 0
        | Some (_, {ponderation; _}) -> ponderation in
      List.fold_left
        (fun max (_, {ponderation; _}) ->
         Js_utils.log "PONDERATION: %i" ponderation;
         if ponderation > max then ponderation else max)
         title_pond
         events
    | Some i -> i in
  object%js
    val exportButton        = tjs_ s_share

    val categoryHeader      = tjs_ s_categories
    val otherFiltersHeader  = tjs_ s_extra_filters
    val panelHeader         = tjs_ s_events
    val minPonderationLabel = tjs_ s_min_ponderation
    val maxPonderationLabel = tjs_ s_max_ponderation
    val filterButtonText    = tjs_ s_filter
    val categoriesText      = tjs_ s_categories
    val ponderationText     = tjs_ s_ponderation
    val andText             = tjs_ s_and
    val betweenText         = tjs_ s_between
    val ponderationHelp     = tjs_ s_ponderation_help
    val editElementHelp     = tjs_ s_edit_element_help
    val addElementHelp      = tjs_ s_add_element_help
    val filterHelp          = tjs_ s_filter_help
    val exportHelp          = tjs_ s_export_help
    val importHelp          = tjs_ s_import_help

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

    val confidentialFilterForm = tjs_ s_confidential_filter_form
 
    val backButton            = tjs_ s_back
    val removeButton          = tjs_ s_remove_event
    val addEditionTokenButton = tjs_ s_add_edition_token_button
    val addReadonlyTokenButton = tjs_ s_add_readonly_token_button

    val formNameAdding        = tjs_ s_add_element_help
    val formNameEditing       = tjs_ s_edit_event
    val addEventButtonText    = tjs_ s_add_new_event
    val updateEventButtonText = tjs_ s_edit_event
    val removeTimelineText = tjs_ s_remove_timeline_text
    
    val editionLinkText = tjs_ s_edition_link_text
    val readLinkText    = tjs_ s_read_link_text

    val timelineListText = tjs_ s_timeline_list_text
    
    val mutable minPonderationFilter =
      match Args.get_min args with
      | None -> 0
      | Some i -> i

    val mutable maxPonderationFilter = max

    val maxBoundInPonderationFilter = max

    val mutable categories     = categories

    val mutable alsoConfidential =
      Js.bool (Args.get_confidential args)

    val mutable timelineName = jss timeline_name
    val mutable newTimelineName = jss timeline_name
    val mutable openedMenu = Js.bool false 

    val mutable startDateFormValue     = jss ""
    val mutable endDateFormValue       = jss ""
    val mutable mediaFormValue         = jss ""
    val mutable headlineFormValue      = jss ""
    val mutable uniqueIdFormValue      = jss ""
    val mutable uniqueIdFormValueDefault = jss ""
    val mutable categoriesFormValue    = jss ""
    val mutable textFormValue          = jss ""
    val mutable tagsFormValue          = jss ""
    val mutable ponderationFormValue   = 0
    val mutable confidentialFormValue  = Js.bool false
    val mutable otherCategoryFormValue = jss ""

    val mutable addingNewEvent = Js.bool false

    val mutable currentEvent = jss ""
    val mutable currentEventInForm = jss ""

    val currentTimeline = jss timeline_id

    val events_ =
      Js.array @@ Array.of_list @@
      List.map
        (fun (_, e) ->
           object%js
             val date =
               jss @@  Format.asprintf "%a" (CalendarLib.Printer.Date.fprint "%D") e.start_date
             val headline = jss e.text.headline
           end
        )
        events

    val mutable filters = Ui_utils.list_to_jsarray @@ List.map filter_to_jsfilter tokens
    val cookieTimelines = Timeline_cookies.js_data ()
  end

type on_page =
  | No_timeline of {name : string; id : string}
  | Timeline of {
      name: string;
      id: string;
      title: (int * title) option;
      events: (int * event) list
    }

let update_filters self tokens =
  self##.filters := Ui_utils.list_to_jsarray @@ List.map filter_to_jsfilter tokens

(* Methods of the view *)

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
    | Some g -> Js_utils.log "Current category is %s" g; self##.categoriesFormValue := jss g in

  let tags_str =
    Format.pp_print_list
      ~pp_sep:(fun fmt _ -> Format.fprintf fmt ",")
      (fun fmt -> Format.fprintf fmt "%s")
      Format.str_formatter
       e.tags;
    Format.flush_str_formatter () in

  let () = (* other fields *)
    self##.headlineFormValue := jss e.text.headline;
    self##.textFormValue := jss e.text.text;
    self##.ponderationFormValue := e.ponderation;
    self##.uniqueIdFormValue := jss e.unique_id;
    self##.confidentialFormValue := Js.bool e.confidential;
    self##.tagsFormValue := jss tags_str in
  ()

let showMenu (self : 'a) : unit = 
  Js_utils.Manip.addClass (Js_utils.find_component "navPanel") "visible";
  self##.openedMenu := Js._true

let showForm title events (self : 'a) (adding : bool) : unit =
  showMenu self;
  Js_utils.Manip.addClass (Js_utils.find_component "formPanel") "visible";
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
  Js_utils.Manip.removeClass (Js_utils.find_component "formPanel") "visible";
  self##.addingNewEvent := (Js.bool false)

let updateTimelineTitle (self : 'a) : unit =
  if self##.timelineName <> self##.newTimelineName then
    let tid = Js.to_string self##.currentTimeline in
    let new_name = Js.to_string self##.newTimelineName in
    ignore @@
    Controller.updateTimelineName
      tid
      new_name
      (fun success ->
         if success then begin
           self##.timelineName := self##.newTimelineName;
           Ui_utils.(
             replace
               (url "" (Args.set_timeline (new_name ^ "-" ^ tid) (Args.get_args ()))))
         end
      )

let hideMenu (self : 'a) : unit =
  hideForm self;
  Js_utils.Manip.removeClass (Js_utils.find_component "navPanel") "visible";
  self##.openedMenu := Js._false;
  updateTimelineTitle self

let addEvent title events self adding : unit =
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
          (fun s ->
             Ui_utils.goto_page (
               Format.sprintf "/edit?timeline=%s-%s#%s"
                 (Js.to_string self##.timelineName)
                 timeline
                 s
             );
             Js_utils.reload ()
          )
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
            ~tags
            ~timeline_id:timeline in
        ()
  end

let removeEvent title events self e =
  let u_id = Js.to_string e in
  let timeline_id = Js.to_string self##.currentTimeline in
  let event_id =
    match List.find_opt (fun (_, e) -> e.unique_id = u_id) events with
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
    let _l : _ Lwt.t = Controller.removeEvent ~id ~timeline_id in
    ()

let removeFromForm title events self =
  let u_id = Js.to_string self##.currentEventInForm in
  let timeline_id = Js.to_string self##.currentTimeline in
  let event_id =
    match List.find_opt (fun (_, e) -> e.unique_id = u_id) events with
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
    let _l : _ Lwt.t = Controller.removeEvent ~id ~timeline_id in
    ()

let export title events _ = Controller.export_timeline title events

let import self =
  let timeline_id = Js.to_string self##.currentTimeline in
  Controller.import_timeline timeline_id true (Js_utils.find_component "import-form")

let addEditionToken self =
  Controller.addToken
    ~readonly:false
    (Js.to_string self##.currentTimeline)
    (update_filters self)

let addReadOnlyToken self =
  Controller.addToken
    ~readonly:true
    (Js.to_string self##.currentTimeline)
    (update_filters self)

let setTokenAsReadOnly self token =
  Controller.updateTokenFilter ~readonly:true (Js.to_string self##.currentTimeline) (Js.to_string token) (update_filters self)

let setTokenAsEdition self token =
  Controller.updateTokenFilter ~readonly:false (Js.to_string self##.currentTimeline) (Js.to_string token) (update_filters self)

let editAlias self filter =
  match Js.Optdef.to_option filter##.editing with
  | None -> (* Entering edition mode *)
    Js_utils.log "New alias treatment: entering edition mode";
    filter##.editing := Js.Optdef.return filter##.pretty
  | Some _f -> (* Sending new pretty alias *)
    Js_utils.log "New alias treatment";
    let pretty =
      let p =  Js.to_string filter##.pretty in
      if p = "" then begin
        Js_utils.log "Removing alias";
        None
      end else begin
        Js_utils.log "New alias: %s" p;
        Some p
      end in
    let tid = Js.to_string self##.currentTimeline in
    let filter_id = Js.to_string filter##.filter_id_ in
      ignore @@
      Controller.updateTokenName
        pretty
        tid
        filter_id
        (fun l ->
           filter##.editing := Js.Optdef.empty;
           update_filters self l)

let removeToken self token =
  Controller.removeToken
    (Js.to_string self##.currentTimeline)
    (Js.to_string token)
    (update_filters self)

let copyLink self readonly filter_id =
  let timeline_name = Js.to_string self##.timelineName in
  let readonly = if Js.to_bool readonly then "view" else "edit" in
  let filter_id = Js.to_string filter_id in
  let link = Format.asprintf "%s%s/%s?timeline=%s-%s"
      (Ui_utils.get_url_prefix ())
      (Ui_utils.get_host ())
      readonly
      timeline_name
      filter_id in
  Js_utils.Clipboard.set_copy ();
  Js_utils.Clipboard.copy link;
  Js_utils.alert "Link copied to clipboard"

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

let filter self =
  try
    let tid, args =
      match Args.get_timeline (Args.get_args ()) with
      | None -> failwith "Error: no timeline!"
      | Some t ->
        (snd (Ui_utils.timeline_id_from_arg t)), Args.set_timeline t [] in
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
      let args = Args.set_min min_ponderation args in
      let max_ponderation = self##.maxPonderationFilter in
      if max_ponderation >= self##.maxBoundInPonderationFilter 
        then args else 
      Args.set_max max_ponderation args
    in
    let args = (* Confidential *)
      let confidential = Js.to_bool self##.alsoConfidential in
      Args.set_confidential confidential args in
    Ui_utils.(push (url "" args));
    ignore @@
    Request.timeline_data ~args tid
      (fun (title, events) -> display_timeline self title events; Lwt.return ())
  with Failure s -> Js_utils.alert s

let displayTokenFilter _self filter =
  let res =
    Js.Optdef.test filter##.after ||
    Js.Optdef.test filter##.before ||
    Js.Optdef.test filter##.min_level_ ||
    Js.Optdef.test filter##.max_level_ ||
    Js.Optdef.test filter##.filterCategories ||
    Js.Optdef.test filter##.tags ||
    not (Js.to_bool filter##.confidential_rights_) in
  res

let removeTimeline self =
  if Js_utils.confirm (Lang.t_ Text.s_confirm_remove_timeline) then ignore @@
    Controller.removeTimeline
      (Js.to_string self##.currentTimeline)
  else ()

let switchToMainInput self =
  Js_utils.(hide (find_component "unique-id-default-form"));
  Js_utils.(show (find_component "unique-id-form"));
  self##.uniqueIdFormValue := self##.uniqueIdFormValueDefault  

let switchToDefaultInput self =
  if Js.to_string self##.uniqueIdFormValue = "" then begin
    self##.uniqueIdFormValue := self##.uniqueIdFormValueDefault;
    Js_utils.(show (find_component "unique-id-default-form"));
    Js_utils.(hide (find_component "unique-id-form"));
  end

let updateDefaultId self =
  let title = Js.to_string self##.headlineFormValue in
  let uid = Js.string @@ Utils.short_title title in
  if self##.uniqueIdFormValue = self##.uniqueIdFormValueDefault then begin
    self##.uniqueIdFormValue := uid
  end;
  self##.uniqueIdFormValueDefault := uid

let first_connexion self =
  let msg =
    Format.sprintf "%s\n\n%s\n\n%s"
      (Lang.t_ Text.s_alert_timeline_creation1)
      (Js.to_string Ocp_js.Dom_html.window##.location##.href)
      (Lang.t_ Text.s_alert_timeline_creation2) in
  Js_utils.alert msg;
  showForm None [] self true

let init
    ~(args: Args.t)
    ~(on_page: on_page)
    ~(categories : (string * bool) list)
    ~(tokens : DbData.filter list) =
  Js_utils.log "Creating vue@.";
  Js_utils.log "Tokens: %i@." (List.length tokens);
  let name, id, events, title =
    match on_page with
    | Timeline {name; id; events; title} -> name, id, events, title
    | No_timeline {name; id} -> name, id, [], None in
  let data = page_vue id name args categories title events tokens in
  Js_utils.log "Adding methods@.";
  Vue.add_method0 "showMenu" showMenu;
  Vue.add_method0 "hideMenu" hideMenu;
  Vue.add_method1 "showForm" (showForm title events);
  Vue.add_method0 "hideForm" hideForm;
  Vue.add_method1 "addEvent" (addEvent title events);
  Vue.add_method1 "removeEvent" (removeEvent title events);
  Vue.add_method0 "removeFromForm" (removeFromForm title events);
  Vue.add_method0 "exportTimeline" (export title events);
  Vue.add_method0 "importTimeline" import;
  Vue.add_method0 "filter" filter;
  Vue.add_method0 "addEditionToken" addEditionToken;
  Vue.add_method0 "addReadOnlyToken" addReadOnlyToken;
  Vue.add_method1 "setTokenAsEdition" setTokenAsEdition;
  Vue.add_method1 "setTokenAsReadOnly" setTokenAsReadOnly;
  Vue.add_method1 "removeToken" removeToken;
  Vue.add_method1 "editAlias" editAlias;
  Vue.add_method2 "copyLink" copyLink;
  Vue.add_method1 "displayTokenFilter" displayTokenFilter;
  Vue.add_method0 "removeTimeline" removeTimeline;
  Vue.add_method0 "switchToMainInput" switchToMainInput;
  Vue.add_method0 "switchToDefaultInput" switchToDefaultInput;
  Vue.add_method0 "updateDefaultId" updateDefaultId;

  Js_utils.log "Adding components@.";
  Js_utils.log "Initializing vue@.";
  let vue = Vue.init ~data (*~show:true*) () in
  let () = Ui_utils.slow_hide (Js_utils.find_component "page_content-loading") in
  let () = FilterNavs.init () in
  let () =
    match on_page with
    | No_timeline {name=_; id} ->
      Js_utils.alert "No timeline has been selected";
      Timeline_cookies.remove_timeline id
    | Timeline {title; events; id; name} ->
      Js_utils.log "Adding timeline to cookies@.";
      let () = Timeline_cookies.add_timeline name id false in
      Js_utils.log "Displaying timeline@.";
      match events with
      | [] -> first_connexion vue
      | _ -> display_timeline vue title events
  in ()
