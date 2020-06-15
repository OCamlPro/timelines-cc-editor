open Ui_utils
open Lwt

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
    method shareNameValue : Js.js_string Js.t Js.prop
    method shareURL : Js.js_string Js.t Js.prop
  end

module Input = struct
  type nonrec data = data
  let id = "page-content"
end

module Vue = Vue_js.Make (Input)

let shareClick self =
  let tid = Js.to_string self##.shareNameValue in
  Controller.viewToken self tid

let init () =
  let data_js : data Js.t =
    object%js
      val logo = jss "EZ-Timeline"
      val navHome = jss "Get started";
      val title = jss "Welcome to EZ-Timeline";
      val subtitle = jss "The simplest way to organize your story";
      val createTimelineTitle = jss "Create your own timeline";
      val createDescr = jss "Click below to create your own timeline. You may enter a name and a short description.";
      val createNamePlaceholder = jss "Name";
      val createDescrPlaceholder = jss "Description";
      val createNameHelp = jss "The name of your timeline (will be used in the URL)";
      val createDescrHelp = jss "A description of your timeline.";
      val createButtonMessage = jss "Start timeline";

      val mutable createNameValue = jss ""
      val mutable createDescrValue = jss ""

      val shareTitle = jss "Share your timeline with everyone";
      val shareDescr = jss "You can share your timeline with others without giving the rights to edit it. Select the timeline you want to share";
      val shareNamePlaceholder = jss "Name";
      val shareNameHelp = jss "The name of the timeline you want to export";
      val mutable shareNameValue = jss "";
      val mutable shareURL = jss "";
    end
  in
  Vue.add_method0
    "createTimeline"
    (fun (self : data Js.t) ->
       Controller.create_timeline
         (Js_of_ocaml.Js.to_string self##.createNameValue)
         (Js_of_ocaml.Js.to_string self##.createDescrValue) >>=
       (fun _ -> Js_utils.log "Ok!"; Lwt.return (Ok ()));
    );
  Vue.add_method0 "shareClick" shareClick;
    

  let _obj = Vue.init ~data_js () in ()
