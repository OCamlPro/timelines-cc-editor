(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

open Lwt

module Js = Js_of_ocaml.Js

class type data = object
  method alertOn : bool Js.t Js.prop
  method promptOn : bool Js.t Js.prop
  method confirmOn : bool Js.t Js.prop
  method message : Js.js_string Js.t Js.prop

  method confirmValue : bool Js.t Js.prop
  method promptValue : Js.js_string Js.t Js.prop

  method ok : unit Js.meth
  method cancel : unit Js.meth
end

let t : data Js.t = object%js(self)
  val mutable alertOn = Js._false
  val mutable promptOn = Js._false
  val mutable confirmOn = Js._false
  val mutable message = Js.string ""
  val mutable confirmValue = Js._false
  val mutable promptValue = Js.string ""

  method ok =
    self##.alertOn := Js._false;
    self##.confirmOn := Js._false;
    self##.promptOn := Js._false;
    self##.confirmValue := Js._true

  method cancel =
    self##.alertOn := Js._false;
    self##.confirmOn := Js._false;
    self##.promptOn := Js._false;
    self##.confirmValue := Js._false;
    self##.promptValue := Js.string ""
end

let alert_lwt (msg : string) : unit Lwt.t =
  t##.alertOn := Js._true;
  t##.message := Js.string msg;
  let promise, resolver = Lwt.wait () in
  let () =
    let rec loop () =
    if Js.to_bool t##.alertOn then
      Js_of_ocaml_lwt.Lwt_js.sleep 0.2 >>= loop
    else begin
      Lwt.wakeup resolver ();
      Lwt.return () end in
    Lwt.async loop in
  promise

let alert (msg : string) : unit =
  let _l : unit Lwt.t = alert_lwt msg in ()

let prompt (msg : string) : string Lwt.t =
  t##.promptOn := Js._true;
  t##.message := Js.string msg;
  let promise, resolver = Lwt.wait () in
  let () =
    let rec loop () =
    if Js.to_bool t##.promptOn then
      Js_of_ocaml_lwt.Lwt_js.sleep 0.5 >>= loop
    else begin
      Lwt.wakeup resolver @@ Js.to_string t##.promptValue;
      Lwt.return () end in
    Lwt.async loop in
  promise

let confirm (msg : string) : bool Lwt.t =
  t##.confirmOn := Js._true;
  t##.message := Js.string msg;
  let promise, resolver = Lwt.wait () in
  let () =
    let rec loop () =
    if Js.to_bool t##.confirmOn then
      Js_of_ocaml_lwt.Lwt_js.sleep 0.5 >>= loop
    else begin
      Lwt.wakeup resolver @@ Js.to_bool t##.confirmValue;
      Lwt.return () end in
    Lwt.async loop in
  promise
