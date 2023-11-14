(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

module Jslang = Ezjs_lang
module StringSet = Utils.Misc.StringSet
module Xhr = Ezjs_xhr

let default_lang = "en"

module Encoding = struct
  open Json_encoding

  let translations =
    obj2
      (req "lang" string)
      (req "translations" (list (tup2 string string)))
      end

let set_lang name =
  Jslang.set ~set:`Cookie name

let () =
  match Jslang.get () with
  | Some lang -> set_lang lang
  | None -> set_lang default_lang

let init draw =
  let download_lang_url lang_file =
    Xhr.get "lang" (Format.sprintf "%s%s:%i/%s" (Ui_utils.get_url_prefix ()) (Ui_utils.get_host ()) (Ui_utils.get_port ()) lang_file)
      (fun res ->
         Ezjs_tyxml.log "Lang OK";
         (try
            let lang, translations =
              EzEncoding.destruct Encoding.translations res
            in
            let set = ref Utils.Misc.StringSet.empty in
            let translations =
              List.fold_left (fun translations (id, txt) ->
                if StringSet.mem id !set then
                  Ezjs_tyxml.log "duplicate id: %S" id;
                set := StringSet.add id !set;
                (id, txt) :: translations) [] translations
            in
            Jslang.add_translations lang translations;
            set_lang lang;
            draw ()
          with exn ->
            Ezjs_tyxml.log
              "Cannot parse lang translations: %s" (Printexc.to_string exn)))
  in
  begin
    match Jslang.get () with
    | None ->
      download_lang_url (Printf.sprintf "lang-%s.json" default_lang)
    | Some lang ->
      download_lang_url (Printf.sprintf "lang-%s.json" lang)
  end

type text = Jslang.string_id
let t_ = Jslang.t_
let s_ = Jslang.s_

let txt_t = Jslang.txt_t
let txt_s = Jslang.txt_s
let ss_ = Jslang.ss_
let id_ = Jslang.id_

let tjs_ s = Ui_utils.jss @@ t_ s
