open Timeline_data
open Data_types
open Utils

exception NewLine of string

let to_event
    ~unique_id
    ~start_date
    ~end_date
    ~typ
    ~typ2:_
    ~ponderation
    ~confidential
    ~media
    ~title
    ~text
    ~last_update
    ~tags
  : date option meta_event =

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
  {
    start_date;
    end_date;
    text;
    group = typ;
    media;
    ponderation;
    confidential;
    unique_id;
    last_update;
    tags
  }

let line_to_event line_id  (header : Header.t) line =
  let data = String.split_on_char '\t' line in
  (*Format.printf "Splitted data = %a@."
    (Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.fprintf fmt ", ") (fun fmt -> Format.fprintf fmt "%s")) data;*)
  let data = Array.of_list data in
  let start_date =
    match Header.start_date header data with
      | None | Some "" -> None
      | Some start -> Utils.string_to_date start
  in
  let ponderation, confidential =
    match Header.confidential header data, Header.importance header data with
    | Some b, None ->
        0, (try bool_of_string b with _ -> true)
    | Some b, Some i -> begin
        try int_of_string i, bool_of_string b with
        | _ -> 0, true
      end
    | None, None -> 0, false
    | None, Some "0" -> 0, true
    | None, Some str ->
      try int_of_string str, false with
      | _ -> 0, false in
  let end_date     = Utils.fopt Utils.string_to_date @@ Header.end_date  header data in
  let typ          = Header.typ         header data in
  let typ2         = Header.typ2        header data in
  let media        = Header.media       header data in
  let title        = Header.title       header data in
  let text         = Header.text        header data in
  let unique_id    =
    match Header.unique_id header data with
    | None -> Format.sprintf "unique-id-%i" line_id
    | Some s -> s in
  to_event
    ~start_date
    ~end_date
    ~typ
    ~typ2
    ~ponderation
    ~media
    ~title
    ~text
    ~confidential
    ~unique_id
    ~last_update:(Some (CalendarLib.Date.today ()))
    ~tags:[]

let date_encoding =
  Json_encoding.(
    conv
      (fun date ->
         (
           CalendarLib.Date.year date,
           CalendarLib.Date.(int_of_month @@ month date),
           Some (CalendarLib.Date.(day_of_month date))
         ))
      (fun (year, month, day) -> Utils.to_date year (Some month) day)
      (obj3
         (req "year" int)
         (req "month" int)
         (opt "day" int)
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

let meta_event_encoding start_date_encoding =
  Json_encoding.(
    conv
      (fun {start_date; end_date; text; group; media; ponderation; confidential; unique_id; last_update; tags} ->
           (start_date, end_date, text, group, media, ponderation, confidential, unique_id, last_update, tags))
      (fun (start_date, end_date, text, group, media, ponderation, confidential, unique_id, last_update, tags) ->
           {start_date; end_date; text; group; media; ponderation; confidential; unique_id; last_update; tags})
      (obj10
         (req "start_date"   start_date_encoding)
         (opt "end_date"     date_encoding)
         (req "text"         text_encoding)
         (opt "group"        group_encoding)
         (opt "media"        media_encoding)
         (req "ponderation"  int)
         (req "confidential" bool)
         (req "unique_id"    string)
         (opt "last_update"  date_encoding)
         (req "tags"         (list string))
      )
  )

let event_encoding = meta_event_encoding date_encoding

let title_encoding = meta_event_encoding (Json_encoding.option date_encoding)

let timeline_encoding =
  Json_encoding.(
    conv
      (fun {events; title} -> (events, title))
      (fun (events, title) -> {events; title})
      (obj2
         (req "events" (list event_encoding))
         (opt "title" title_encoding))
  )

let id_timeline_encoding =
  Json_encoding.(
    obj2
      (req "events" (list event_encoding))
      (opt "title" title_encoding))

let file_to_events f =
  let chan = open_in f in
  let title = to_title @@ input_line chan in
  let header = Header.header_to_map @@ input_line chan in
  (* Format.printf "Header = %a@." Header.pp header;*)
  let events =
    let l = ref [] in
    let cpt = ref 0 in
    let () =
      try
        while true do
          let line = input_line chan in
          (* Format.printf "Line %s@." line;*)
          try
            cpt := !cpt + 1;
            match metaevent_to_event @@ line_to_event !cpt header line with
            | None -> Format.printf "Event %s has no start date" line
            | Some event -> l := (event :: !l)
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
  {title = Some title; events}

let file_to_json f =
  let timeline = file_to_events f in
  Json_encoding.construct timeline_encoding timeline

let str_to_events ~log_error str =
  let loop = function
    | [] -> failwith "Empty file"
    | [title] -> {title = Some (to_title title) ; events = []}
    | title :: header :: tl ->
      let title = to_title title in
      let header = Header.header_to_map header in
      let events =
        let l = ref [] in
        let cpt = ref 0 in
        let () =
          List.iter
            (fun line ->
               cpt := !cpt + 1;
               try
                 match metaevent_to_event @@ line_to_event !cpt header line with
                 | None -> Format.printf "Event %s has no start date" line
                 | Some new_event -> l := new_event :: !l
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
      in {title = Some title; events}
  in
loop @@ String.split_on_char '\n' str

let write_json json f =
  let chan = open_out f in
  let yojson = Json_repr.to_yojson json in
  let str = Format.asprintf "%a" (Json_repr.pp (module Json_repr.Yojson)) yojson in
  output_string chan str;
  close_out chan

let title_to_csv ~sep (title : title) =
  Format.asprintf
    "%s%s%s"
    title.text.headline
    sep
    title.text.text

let event_to_csv ~sep (event : event) =
  Format.asprintf (* start date, end_date, media, group, confidential, ponderation, title, text *)
    "%a%s\
     %a%s\
     %a%s\
     %a%s\
     %b%s\
     %i%s\
     \"%s\"%s\
     \"%s\""
    (CalendarLib.Printer.Date.fprint "%F") event.start_date sep
    (Utils.pp_opt (CalendarLib.Printer.Date.fprint "%F")) event.end_date sep
    (Utils.pp_opt (fun fmt {url} -> Format.fprintf fmt "%s" url)) event.media sep
    (Utils.pp_opt (fun fmt -> Format.fprintf fmt "%s")) event.group sep
    event.confidential sep
    event.ponderation sep
    event.text.headline sep
    event.text.text
