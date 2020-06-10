module Js = Js_of_ocaml.Js

class type categoryFilter = object
  method id : int Js.readonly_prop
  method catName : Js.js_string Js.t Js.readonly_prop
  method catId   : Js.js_string Js.t Js.readonly_prop
  method checked : bool Js.t Js.prop
end

class type data = object
    method editSlide : Js.js_string Js.t Js.prop
    method exportButton : Js.js_string Js.t Js.readonly_prop
    method adminButton : Js.js_string Js.t Js.readonly_prop

    method categoryHeader : Js.js_string Js.t Js.readonly_prop
    method otherFiltersHeader : Js.js_string Js.t Js.readonly_prop
    method panelHeader : Js.js_string Js.t Js.readonly_prop
    method minPonderationLabel : Js.js_string Js.t Js.readonly_prop
    method maxPonderationLabel : Js.js_string Js.t Js.readonly_prop

    method ponderationHelp : Js.js_string Js.t Js.readonly_prop

    method filterButtonText : Js.js_string Js.t Js.readonly_prop
    
    method minPonderation : int Js.prop
    method maxPonderation : int Js.prop

    method categories : categoryFilter Js.t Js.js_array Js.t Js.prop


  end

module Input = struct
  type nonrec data = data
  let id = "page-content"
end

module Vue = Vue_js.Make (Input)

let jss = Js.string

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
    ~(title : (int * Data_types.title) option)
    ~(events : (int * Data_types.event) list)
    ~(categories : (string * bool) list) =

  (* First, displaying timeline *)
  display_timeline title events;
  
  let data_js : data Js.t =
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
    in
    
    object%js
      val mutable editSlide = jss "Edit slide"
      val exportButton = jss "Share timeline"
      val adminButton = jss "Administration panel"

      val categoryHeader = jss "Categories"
      val otherFiltersHeader = jss "Extra filters"
      val panelHeader = jss "Events"
      val minPonderationLabel = jss "Minimal Ponderation"
      val maxPonderationLabel = jss "Max Ponderation"
      val ponderationHelp = jss "Select two values"
      val filterButtonText = jss "Filter"

      val mutable minPonderation = 0
      val mutable maxPonderation = 100
      val mutable categories = categories
    end
  in
  let _cat = category_component () in
  let _obj = Vue.init ~data_js () in ()
