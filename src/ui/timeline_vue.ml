open Data_types
open Ui_utils

module Js = Js_of_ocaml.Js


class type categoryFilter = object
  method id : int Js.readonly_prop
  method catName : Js.js_string Js.t Js.readonly_prop
  method catId   : Js.js_string Js.t Js.readonly_prop
  method checked : bool Js.t Js.prop
end

class type data = object
    method exportButton : Js.js_string Js.t Js.readonly_prop
    method adminButton  : Js.js_string Js.t Js.readonly_prop

    method categoryHeader      : Js.js_string Js.t Js.readonly_prop
    method otherFiltersHeader  : Js.js_string Js.t Js.readonly_prop
    method panelHeader         : Js.js_string Js.t Js.readonly_prop
    method minPonderationLabel : Js.js_string Js.t Js.readonly_prop
    method maxPonderationLabel : Js.js_string Js.t Js.readonly_prop

    method ponderationHelp : Js.js_string Js.t Js.readonly_prop

    method filterButtonText : Js.js_string Js.t Js.readonly_prop
    
    method minPonderation : int Js.prop
    method maxPonderation : int Js.prop

    method categories : categoryFilter Js.t Js.js_array Js.t Js.prop

    method fromFormTitle         : Js.js_string Js.t Js.readonly_prop
    method toFormTitle           : Js.js_string Js.t Js.readonly_prop
    method mediaFormTitle        : Js.js_string Js.t Js.readonly_prop
    method headlineFormTitle     : Js.js_string Js.t Js.readonly_prop
    method uniqueIdFormTitle     : Js.js_string Js.t Js.readonly_prop
    method categoriesFormTitle   : Js.js_string Js.t Js.readonly_prop
    method textFormTitle         : Js.js_string Js.t Js.readonly_prop
    method tagsFormTitle         : Js.js_string Js.t Js.readonly_prop
    method ponderationFormTitle  : Js.js_string Js.t Js.readonly_prop
    method confidentialFormTitle : Js.js_string Js.t Js.readonly_prop
    method backButton            : Js.js_string Js.t Js.readonly_prop
        
    method updateEventButton     : Js.js_string Js.t Js.prop
    method formName              : Js.js_string Js.t Js.prop
end

module PageContent = struct
  type nonrec data = data
  let id = "page-content"
end

module Vue = Vue_js.Make (PageContent)

let page_vue (categories : (string * bool) list) : data Js.t =
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
    val ponderationHelp     = jss "Select two values"
    val filterButtonText    = jss "Filter"

    val mutable minPonderation = 0
    val mutable maxPonderation = 100
    val mutable categories     = categories

    val fromFormTitle         = jss "From"
    val toFormTitle           = jss "To"
    val mediaFormTitle        = jss "Media"
    val headlineFormTitle     = jss "Headline"
    val uniqueIdFormTitle     = jss "Unique"
    val categoriesFormTitle   = jss "Category"
    val textFormTitle         = jss "Description"
    val tagsFormTitle         = jss "Tags (separate with ',')"
    val ponderationFormTitle  = jss "Ponderation"
    val confidentialFormTitle = jss "Draft"
    val backButton            = jss "Back"

    val mutable formName          = jss "Add a new event"
    val mutable updateEventButton = jss "Add event"
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

(* Timeline initializer *)
let display_timeline title events =  
  let timeline =
    let title =
      match title with
      | None -> None
      | Some (_, t) -> Some t in
    let events = List.map snd events in
    {Data_types.events; title} in
  let json = Json_encoding.construct (Data_encoding.timeline_encoding) timeline in
  let yoj  = Json_repr.to_yojson json in
  let str  = Yojson.Safe.to_string yoj in (*
  let () =
    Js_of_ocaml.Js.Unsafe.js_expr @@
    Format.asprintf
      "window.timeline = new TL.Timeline('home-timeline-embed',%s)"
      str in () *)
  Timeline.make "home-timeline-embed" str

let init
    ~(on_page: on_page)
    ~(categories : (string * bool) list) =

  (* First : displaying titles *)

  let data_js = page_vue categories in
  
  (* Adding methods *)
  Vue.add_method0
    "showForm"
    (fun _ -> Js_utils.Manip.addClass (Js_utils.find_component "navPanel") "visible");
  Vue.add_method0
    "hideForm"
    (fun _ -> Js_utils.Manip.removeClass (Js_utils.find_component "navPanel") "visible");
  
  let _cat = category_component () in
  let _obj = Vue.init ~data_js () in

  (* Now displaying timeline *)

  let () =
    match on_page with
    | No_timeline -> Js_utils.alert "No timeline has been selected"
    | Timeline {title; events; name} -> display_timeline title events in
  ()
