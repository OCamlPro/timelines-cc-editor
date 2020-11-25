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
    method emailPlaceholder : Js.js_string Js.t Js.readonly_prop
    method emailHelp : Js.js_string Js.t Js.readonly_prop
    method createNameHelp : Js.js_string Js.t Js.readonly_prop
    method createDescrHelp : Js.js_string Js.t Js.readonly_prop
    method createButtonMessage : Js.js_string Js.t Js.readonly_prop

    method createNameValue : Js.js_string Js.t Js.prop
    method createDescrValue : Js.js_string Js.t Js.prop
    method emailValue : Js.js_string Js.t Js.prop

    method shareTitle : Js.js_string Js.t Js.readonly_prop
    method shareDescr : Js.js_string Js.t Js.readonly_prop

    method securityTitle : Js.js_string Js.t Js.readonly_prop
    method securityDescr : Js.js_string Js.t Js.readonly_prop

    method disableCookiesButtonText : Js.js_string Js.t Js.readonly_prop
    method enableCookiesButtonText : Js.js_string Js.t Js.readonly_prop

    method cookiesEnabled : bool Js.t Js.prop

    method cookieTimelines : Timeline_cookies.urlData Js.t Js.js_array Js.t Js.prop
  end

module Input = struct
  type nonrec data = data
  type all = data
  let id = "page_content"
end

module Vue = Vue_js.Make (Input)

let createTimeline (self : data Vue_js.vue) =
  let name = Js_of_ocaml.Js.to_string self##.createNameValue in
  let email =
    match Js_utils.Window.prompt (Lang.t_ Text.s_prompt_ask_email) with
    | "" -> None
    | m -> Some m in
  ignore @@
  Controller.create_timeline
    ?email
    name
    (Js_of_ocaml.Js.to_string self##.createDescrValue)
    (fun ~name ~id ->
      let name =
        match name with
        | None -> id
        | Some n -> n in
      let () = Timeline_cookies.add_timeline name id false in
      let id = Ui_utils.timeline_arg_from_id ~name id in
      let new_page = Format.sprintf "/edit?timeline=%s" id in
      Js_utils.log "Going to %s" new_page;
      Ui_utils.goto_page new_page
    )

let enableCookies self =
  Timeline_cookies.enable ();
  self##.cookiesEnabled := Js._true

let disableCookies self =
  if Js_utils.confirm (Lang.t_ s_confirm_disable_cookies) then begin
    Timeline_cookies.disable ();
    self##.cookiesEnabled := Js._false
  end

let removeTimelineFromCookies self id =
  Timeline_cookies.remove_timeline @@ Js.to_string id;
  let cookie_timelines, _ = Timeline_cookies.js_data () in
  self##.cookieTimelines := cookie_timelines

let init () =
  let cookieTimelines, enabled = Timeline_cookies.js_data () in
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
      val emailPlaceholder = tjs_ s_email_placeholder
      val emailHelp = tjs_ s_email_help
      val createNameHelp = tjs_ s_create_timeline_name_help
      val createDescrHelp = tjs_ s_create_timeline_descr_help
      val createButtonMessage = tjs_ s_create_timeline_button

      val shareTitle = tjs_ s_share_title
      val shareDescr = tjs_ s_share_descr

      val securityTitle = tjs_ s_security_title
      val securityDescr = tjs_ s_security_descr

      val enableCookiesButtonText = tjs_ s_enable_cookies_button_text
      val disableCookiesButtonText = tjs_ s_disable_cookies_button_text

      val mutable createNameValue = jss ""
      val mutable createDescrValue = jss ""
      val mutable emailValue = jss ""

      val mutable cookiesEnabled = Js.bool enabled
      val mutable cookieTimelines = cookieTimelines
    end
  in
  Vue.add_method0 "createTimeline" createTimeline;
  Vue.add_method1 "removeTimelineFromCookies" removeTimelineFromCookies;
  Vue.add_method0 "enableCookies" enableCookies;
  Vue.add_method0 "disableCookies" disableCookies;

  let _obj = Vue.init ~data () in
  Ui_utils.slow_hide (Js_utils.find_component "page_content-loading");
  ()
