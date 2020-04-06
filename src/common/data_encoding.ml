open Data_types
open Utils

(* Format évènement:
   * Debut année
   * Debut mois
   * Fin année
   * Fin mois
   * Type (logiciel, personne, client, + OCaml, blockchain, partenariat/écosystème, etc.)
   * Type 2 (à déterminer)
   * Pondération (0 = confidentiel à masquer)
   * Lien (blogpost, github, lien video etc.)
   * Titre
   * Narration (optionnel)   *)

exception NewLine of string

let to_event
    start_date
    end_date
    typ
    typ2
    importance
    media
    title
    text =

  let text =
    let title =
      match title with
      | None -> ""
      | Some t -> t in
    let text =
      match text with
      | None -> ""
      | Some t -> t in
    to_text title text
  in

  let media = opt to_media media in

  let end_date =
    match end_date with
    | None -> Some start_date
    | Some res -> Some res
  in {
    start_date;
    end_date;
    text;
    group = typ;
    media
  }

let line_to_event (header : Header.t) line =
  let data = String.split_on_char '\t' line in
  (*Format.printf "Splitted data = %a@."
    (Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.fprintf fmt ", ") (fun fmt -> Format.fprintf fmt "%s")) data;*)
  let data = Array.of_list data in
  let start_date =
    match Header.start_year  header data with
      | None -> failwith "There should be a start year for an event"
      | Some start_year ->
        let start_month =
          match opt int_of_string (Header.start_month header data) with
            None -> None
          | res -> res
          | exception Failure _ -> None in
        try
          to_date (int_of_string start_year) start_month None
        with Failure _ -> raise (NewLine ("Error trying to parse start year " ^ start_year))
  in
  let end_date =
    match Header.end_year header data with
    | None | Some "" -> None
    | Some end_year ->
      let end_month =
        match opt int_of_string (Header.end_month header data) with
          None -> None
        | res -> res
        | exception Failure _ -> None
      in
      try
        Some (to_date (int_of_string end_year) end_month None)
      with
        Failure _ ->
        failwith ("Error trying to parse end year year " ^ end_year)
  in
  let typ         = Header.typ         header data in
  let typ2        = Header.typ2        header data in
  let importance  = Header.importance  header data in
  let media       = Header.media       header data in
  let title       = Header.title       header data in
  let text        = Header.text        header data in
  to_event
    start_date
    end_date
    typ
    typ2
    importance
    media
    title
    text

let to_title line =
  match String.split_on_char '\t' line with
  | title :: text :: _ -> to_text title text
  | _ -> raise (Invalid_argument (Format.sprintf "Missing elements for building title (%s)" line))

let date_encoding =
  Json_encoding.(
    conv
      (fun date -> (CalendarLib.Date.year date, CalendarLib.Date.(int_of_month @@ month date)))
      (fun (year, month) -> Utils.to_date year (Some month) None)
      (obj2
         (req "year" int)
         (req "month" int)
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

let group_encoding = Json_encoding.string

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
         (opt "group"      group_encoding)
         (opt "media"      media_encoding)
      )
  )

let title_encoding = Json_encoding.(obj1 (req "text" text_encoding))

let timeline_encoding =
  Json_encoding.(
    conv
      (fun {events; title} -> (events, title))
      (fun (events, title) -> {events; title})
      (obj2
         (req "events" (list event_encoding))
         (req "title" title_encoding))
  )

let file_to_events f =
  let chan = open_in f in
  let title = to_title @@ input_line chan in
  let header = Header.header_to_map @@ input_line chan in
  (* Format.printf "Header = %a@." Header.pp header;*)
  let events =
    let l = ref [] in
    let () =
      try
        while true do
          let line = input_line chan in
          (* Format.printf "Line %s@." line;*)
          try
            let event = line_to_event header line in
            l := event :: !l;
            (* Format.printf
              "Event of line %s started on %a and ended on %s@."
              line
              (CalendarLib.Printer.Date.fprint "%D") event.start_date
              (match event.end_date with
               | None -> ""
               | Some i -> Format.asprintf "and ended on %a" (CalendarLib.Printer.Date.fprint  "%D") i) *)
          with
            Failure s -> Format.printf "Failing at line %s: %s@." line s
          | NewLine s -> begin
              match !l with
              | hd :: tl ->
                let text = hd.text in
                l := {hd with text = {text with text = text.text ^ "\n" ^ line}} :: tl
              | [] ->
                failwith
                  (Format.sprintf "First line %s is incorrect: %s" line s)
            end
        done
      with End_of_file -> ()
    in !l
  in
  close_in chan;
  {title; events}

let file_to_json f =
  let timeline = file_to_events f in
  Json_encoding.construct timeline_encoding timeline

let str_to_events ~log_error str =
  let rec loop = function
    | [] -> failwith "Empty file"
    | [title] -> {title = to_title title ; events = []}
    | title :: header :: tl ->
      let title = to_title title in
      let header = Header.header_to_map header in
      let events =
        let l = ref [] in
        let () =
          List.iter
            (fun line ->
               try
                 let new_event = line_to_event header line in
                 l := new_event :: !l
               with
               | NewLine s -> begin
                   match !l with
                   | hd :: tl ->
                     let text = hd.text in
                     l := {hd with text = {text with text = text.text ^ "\n" ^ line}} :: tl
                   | [] ->
                     let error = (Format.sprintf "First line %s is incorrect: %s" line s) in
                     log_error line s;
                     failwith error
                 end
               | Failure s -> log_error line s
               | _ -> log_error line "Exception"
            ) tl
        in !l
  in {title; events}
in
loop @@ String.split_on_char '\n' str

let write_json json f =
  let chan = open_out f in
  let yojson = Json_repr.to_yojson json in
  let str = Format.asprintf "%a" (Json_repr.pp (module Json_repr.Yojson)) yojson in
  output_string chan str;
  close_out chan
