open Data_types

let to_int_opt s =
  if s = "" then None
  else Some (int_of_string s)

let to_date year month = {
  year  = int_of_string year;
  month = to_int_opt month
}

let to_date_opt year month =
  if year = "" then None
  else Some (to_date year month)

let to_text headline text subtyp level =
  let id = if subtyp = "" then "" else ("id = '" ^ subtyp ^ "'") in
  let cl = if level  = "" then "" else ("class = '" ^ level ^ "'") in
  let text =
    Format.asprintf
      "<div %s %s>%s</div>"
      id
      cl
      text
  in
  {
    headline;
    text
  }

let to_type s =
  match String.lowercase_ascii s with
  | "soft" -> Some Software
  | "person" -> Some Person
  | "client" -> Some Client
  | _ -> None

let type_to_str = function
  | Some Software -> "Soft"
  | Some Person -> "Person"
  | Some Client -> "Client"
  | _ -> ""

let to_media url =
  if url = "" then
    None
  else Some {url}

(* Format évènement:
   * Debut
   * Debut mois
   * Fin
   * Fin mois
   * Type (logiciel, personne, client)
   * Type 2 (à déterminer)
   * Pondération (important ou pas)
   * Image ou lien video ou github
   * Titre
   * Narration (optionnel)   *)
let to_event line =
  match String.split_on_char '\t' line with
  | start_year :: start_month ::
    end_year :: end_month ::
    main_typ :: sub_typ ::
    level :: url :: title :: text :: _ ->
    {
      start_date = to_date start_year start_month;
      end_date   = to_date_opt end_year end_month;
      text = to_text title text sub_typ level;
      group = to_type main_typ;
      media = to_media url
    }
  | _ -> raise (Invalid_argument (Format.sprintf "Missing elements for building event (%s)" line))

let to_title line =
  match String.split_on_char '\t' line with
  | title :: text :: _ -> to_text title text "" ""
  | _ -> raise (Invalid_argument (Format.sprintf "Missing elements for building title (%s)" line))

let date_encoding =
  Json_encoding.(
    conv
      (fun {year; month} -> (year, month))
      (fun (year, month) -> {year; month})
      (obj2
         (req "year" int)
         (opt "month" int)
      )
  )

let text_encoding = Json_encoding.(
    conv
      (fun {headline; text} -> (headline, text))
      (fun (headline, text) -> {headline; text})
      (obj2
         (req "headline" string)
         (req "text" string)))

let media_encoding =
  Json_encoding.(
    conv
      (fun {url} -> url)
      (fun url -> {url})
      (obj1 (req "url" string))
  )

let group_encoding =
  Json_encoding.(
    conv
      type_to_str
      to_type
      string
  )

let event_encoding =
  Json_encoding.(
    conv
      (fun {start_date; end_date; text; group; media} ->
           (start_date, end_date, text, group, media))
      (fun (start_date, end_date, text, group, media) ->
           {start_date; end_date; text; group; media})
      (obj5
         (req "start_date" date_encoding)
         (opt "end_date"   date_encoding)
         (req "text"       text_encoding)
         (req "group"      group_encoding)
         (opt "media"      media_encoding)
      )
  )

let timeline_encoding =
  Json_encoding.(
    conv
      (fun {events; title} -> (events, title))
      (fun (events, title) -> {events; title})
      (obj2
         (req "events" (list event_encoding))
         (req "title" text_encoding))
  )

let file_to_json f =
  let chan = open_in f in
  let title = to_title @@ input_line chan in
  let events =
    let l = ref [] in
    let () =
      try
        while true do
          let line = input_line chan in
          try
            l := to_event line :: !l
          with Failure s -> Format.printf "Failing at line %s: %s" line s
        done
      with End_of_file -> ()
    in !l
  in
  close_in chan;
  let timeline = {title; events} in
  Json_encoding.construct timeline_encoding timeline

let write_json json f =
  let chan = open_out f in
  let yojson = Json_repr.to_yojson json in
  let str = Format.asprintf "%a" (Json_repr.pp (module Json_repr.Yojson)) yojson in
  output_string chan str;
  close_out chan

let read_json f =
  let yojson = Yojson.Safe.from_file f in
  let ezjsonm = Json_repr.from_yojson yojson in
  Json_encoding.destruct timeline_encoding ezjsonm
