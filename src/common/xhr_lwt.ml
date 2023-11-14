(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(*                                                                           *)
(* Copyright (c) 2019 Origin Labs - contact@origin-labs.com                  *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining     *)
(* a copy of this software and associated documentation files (the           *)
(* "Software"), to deal in the Software without restriction, including       *)
(* without limitation the rights to use, copy, modify, merge, publish,       *)
(* distribute, sublicense, and/or sell copies of the Software, and to        *)
(* permit persons to whom the Software is furnished to do so, subject to     *)
(* the following conditions:                                                 *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be            *)
(* included in all copies or substantial portions of the Software.           *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,           *)
(* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF        *)
(* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                     *)
(* NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE    *)
(* LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION    *)
(* OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION     *)
(* WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.           *)
(*                                                                           *)
(*****************************************************************************)

module Xhr = Js_of_ocaml_lwt.XmlHttpRequest
open Xhr
open Js_of_ocaml.Url
open Lwt
open Json_encoding

type 'a error =
  | Xhr_err of string generic_http_frame
  | Str_err of string
  | Gen_err of (int * string)
  | Exn_err of exn list

let error_content = function
  | Xhr_err f -> (f.code, f.content)
  | Str_err s -> (42, s)
  | Gen_err c -> c
  | Exn_err e -> (42, String.concat "\n" @@ List.map Printexc.to_string e)

let base ?port ?(scheme="https") hu_host =
  let hu_port = match port with
    | Some port -> port
    | None -> match scheme with
      | "https" -> 443
      | _ -> 80 in
  let url = {
    hu_host; hu_port; hu_path = []; hu_path_string = ""; hu_arguments = [];
    hu_fragment = "" } in
  match scheme with
  | "https" -> Https url
  | _ -> Http url

let make_url ~base ?(args=[]) url =
  let scheme, base = match base with
    | Http base -> "http", base
    | Https base -> "https", base
    | _ -> assert false in
  let hu_path_string = base.hu_path_string ^ url in
  let hu_path = path_of_path_string hu_path_string in
  match scheme with
  | "http" -> Http {base with hu_path_string; hu_path; hu_arguments=args}
  | "https" -> Https {base with hu_path_string; hu_path; hu_arguments=args}
  | _ -> assert false

let handle_response frame =
  (* Format.eprintf "RESPONSE @[%s@]@." frame.content; *)
  if frame.code = 200 then return (Ok frame.content)
  else return (Error (Xhr_err frame))

let make_frame ?(code=0) ?(url="") ?(headers=fun _ -> None)
    ?(content_xml=fun _ -> None) content =
  {url; code; headers; content; content_xml}

let get_raw ~base ?args url =
  let url2 = make_url ~base ?args url in
  (* Format.eprintf "GET %s@." (string_of_url url2); *)
  try Xhr.perform url2 >>= handle_response
  with _ -> return (Error (Xhr_err (make_frame ~url ("No response from server"))))

let post_raw ~base ?args ?(content_type="application/json") url contents =
  let url2 = make_url ~base ?args url in
  (* Format.eprintf "POST %s with @[%s@]@." (string_of_url url2) contents; *)
  let contents = `String contents in
  try Xhr.perform ~content_type ~contents url2 >>= handle_response
  with _ -> return (Error (Xhr_err (make_frame ~url ("No response from server for " ^ url))))

let cook ?(eprint = Format.eprintf) encs =
  (function
    | Ok res -> begin
        let js = Ezjsonm.from_string res in
        let res =
          List.fold_left
            (fun acc enc ->
               match acc with
               | Some _ -> acc
               | None ->
                 try Some (destruct enc js) with
                   Json_encoding.Cannot_destruct (_p, e) ->
                   let str_err =
                     Format.asprintf "%a" (Json_query.print_error ?print_unknown:None) e in
                   eprint "[Xhr_lwt.post] Code %i: %s@." 42 str_err;
                   None
            )
            None
            encs
        in
        match res with
        | None ->
          return (Error (Str_err "Error: cannot destruct value"))
        | Some res -> return (Ok res)
      end
    | Error e ->
      let code, error = error_content e in
      eprint "[Xhr_lwt.post] Error %i: %s@." code error; return (Error e))

let get ~base ?(eprint = Format.eprintf) ?args url output_encs =
  get_raw ~base ?args url >>=
  (cook ~eprint output_encs)

let post ~base ?(eprint = Format.eprintf) ?args ?content_type input_enc output_encs url contents =
  let contents = construct input_enc contents in
  let contents = Format.asprintf "%a" (Json_repr.pp_any ()) (Json_repr.to_any contents) in
  post_raw ~base ?args ?content_type url contents >>=
  (cook ~eprint output_encs)

let pp_err fmt e =
  let code, err = error_content e in
  Format.fprintf fmt "Error %i: %s" code err
