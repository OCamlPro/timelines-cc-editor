open Timeline_data

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
    Xhr.get "lang" (Format.sprintf "%s%s/%s" (Ui_utils.get_url_prefix ()) (Ui_utils.get_host ()) lang_file)
      (fun res ->
         Js_utils.log "Lang OK";
         (try
            let lang, translations =
              EzEncoding.destruct Encoding.translations res
            in
            let set = ref Utils.StringSet.empty in
            let translations =
              List.fold_left (fun translations (id, txt) ->
                if Utils.StringSet.mem id !set then
                  Js_utils.log "duplicate id: %S" id;
                set := Utils.StringSet.add id !set;
                (id, txt) :: translations) [] translations
            in
            Jslang.add_translations lang translations;
            set_lang lang;
            draw ()
          with exn ->
            Js_utils.log
              "Cannot parse lang translations: %s" (Printexc.to_string exn)))
  in
  begin
    match Jslang.get () with
    | None -> 
      download_lang_url (Printf.sprintf "lang-%s.json" default_lang)
    | Some lang ->
      download_lang_url (Printf.sprintf "lang-%s.json" lang)
  end (* ;
  begin
    match Jsloc.find_arg "lang_url" with
    | None -> ()
    | Some lang_url ->
      download_lang_url lang_url
  end *)

type text = Jslang.string_id
let t_ = Jslang.t_
let s_ = Jslang.s_

let txt_t = Jslang.txt_t
let txt_s = Jslang.txt_s
let ss_ = Jslang.ss_
let id_ = Jslang.id_

let tjs_ s = Ui_utils.jss @@ t_ s 
