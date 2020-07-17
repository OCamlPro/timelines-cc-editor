open Lwt
open Ui_common
open Ui_utils
open Text
open Lang

module Js = Js_of_ocaml.Js

class type data = object
    method logo : Js.js_string Js.t Js.readonly_prop
    method navHome : Js.js_string Js.t Js.readonly_prop
    method title : Js.js_string Js.t Js.readonly_prop
    method subtitle : Js.js_string Js.t Js.readonly_prop

    method createTimelineTitle : Js.js_string Js.t Js.readonly_prop
    method createDescr : Js.js_string Js.t Js.readonly_prop
    method createNamePlaceholder : Js.js_string Js.t Js.readonly_prop
    method createDescrPlaceholder : Js.js_string Js.t Js.readonly_prop
    method createNameHelp : Js.js_string Js.t Js.readonly_prop
    method createDescrHelp : Js.js_string Js.t Js.readonly_prop
    method createButtonMessage : Js.js_string Js.t Js.readonly_prop

    method createNameValue : Js.js_string Js.t Js.prop
    method createDescrValue : Js.js_string Js.t Js.prop

    method shareTitle : Js.js_string Js.t Js.readonly_prop
    method shareDescr : Js.js_string Js.t Js.readonly_prop
    method shareNamePlaceholder : Js.js_string Js.t Js.readonly_prop
    method shareNameHelp : Js.js_string Js.t Js.readonly_prop
    method shareNameButton : Js.js_string Js.t Js.readonly_prop
    method shareNameValue : Js.js_string Js.t Js.prop
    method shareURL : Js.js_string Js.t Js.prop
  end

module Input = struct
  type nonrec data = data
  type all = data
  let id = "page_content"
end

module Vue = Vue_js.Make (Input)

let createTimeline (self : data Vue_js.vue) =
  Controller.create_timeline
    (Js_of_ocaml.Js.to_string self##.createNameValue)
    (Js_of_ocaml.Js.to_string self##.createDescrValue) >>=
  (fun _ -> Js_utils.log "Ok!"; Lwt.return (Ok ()))

(*
let shareClick self =
  let tid = Js.to_string self##.shareNameValue in
  Controller.viewToken self tid
*)

let init () =
  let data : data Js.t =
    object%js
      val logo = tjs_ s_ez_timeline
      val navHome = tjs_ s_nav_home
      val title = tjs_ s_home_title
      val subtitle = tjs_ s_home_subtitle
      val createTimelineTitle = tjs_ s_create_timeline_title
      val createDescr = tjs_ s_create_timeline_descr
      val createNamePlaceholder = tjs_ s_name
      val createDescrPlaceholder = tjs_ s_description
      val createNameHelp = tjs_ s_create_timeline_name_help
      val createDescrHelp = tjs_ s_create_timeline_descr_help
      val createButtonMessage = tjs_ s_create_timeline_button

      val shareTitle = tjs_ s_share_title
      val shareDescr = tjs_ s_share_descr
      val shareNamePlaceholder = tjs_ s_name
      val shareNameHelp = tjs_ s_share_help
      val shareNameButton = tjs_ s_share

      val mutable createNameValue = jss ""
      val mutable createDescrValue = jss ""

      val mutable shareNameValue = jss ""
      val mutable shareURL = jss ""
    end
  in
  Vue.add_method0 "createTimeline" createTimeline;(*
  Vue.add_method0 "shareClick" shareClick; *)

  let _obj = Vue.init ~data () in
  Ui_utils.slow_hide (Js_utils.find_component "page_content-loading");
  ()
