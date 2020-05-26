open Data_types

let raw_data = "data"

let full_data = "data.json"

let timeline_data = "data_raw.json"

let to_int_opt s =
  if s = "" then None
  else try Some (int_of_string s) with Invalid_argument _ -> None

let to_text headline text =
  {
    headline;
    text
  }

let short_title str =
  let allowed_char c =
    let code = Char.code c in
    if (code >= 65 && code <= 90)  || (* between 'A' and 'Z' *)
       (code >= 97 && code <= 122) || (* between 'a' and 'z' *)
       (code >= 48 && code <= 57)  (* between '0' and '9' *)
    then c
    else
      match c with
      | '.' | '-' | '_' -> c
      | _ -> '-' in
  if str = "" then
    "__no_title__id__"
  else
    let short =
      String.sub str 0 (min (String.length str) 60) in
    String.map allowed_char short

let to_title_event headline text = {
  start_date = None;
  end_date = None;
  text = {headline; text};
  group = None;
  media = None;
  ponderation = 0;
  confidential = false;
  unique_id = "timeline-title-" ^ (short_title headline);
  last_update = None
}

let to_title line =
  match String.split_on_char '\t' line with
  | title :: text :: _ -> to_title_event title text
  | _ -> raise (Invalid_argument (Format.sprintf "Missing elements for building title (%s)" line))

let metaevent_to_event meta : event option =
  match meta.start_date with
  | None -> None
  | Some start_date -> Some {
      start_date;
      end_date = meta.end_date;
      text = meta.text;
      media = meta.media;
      group = meta.group;
      confidential = meta.confidential;
      ponderation = meta.ponderation;
      unique_id = meta.unique_id;
      last_update = None
    }

let event_to_metaevent e = {
      start_date = Some e.start_date;
      end_date = e.end_date;
      text = e.text;
      media = e.media;
      group = e.group;
      confidential = e.confidential;
      ponderation = e.ponderation;
      unique_id = e.unique_id;
      last_update = None
    }

let to_date year month day =
  let default_first = function
      None -> 1
    | Some e -> e in
  let month = default_first month in
  let day   = default_first day   in
  CalendarLib.Date.make year month day

let string_to_date str =
  try
    match String.split_on_char '-' str with
    | [] -> None
    | [year] -> Some (to_date (int_of_string year) None None)
    | [year; month] -> Some (to_date (int_of_string year) (Some (int_of_string month)) None)
    | year :: month :: day :: _ ->
      Some (
        to_date
          (int_of_string year)
          (Some (int_of_string month))
          (Some (int_of_string day))
      )
  with Failure _ (* int_of_string *) -> None

let to_media url = {url}

let opt f = function
  | None -> None
  | Some e -> Some (f e)


module StringMap = Map.Make (String)
module IntMap = Map.Make (struct type t = int let compare = (-) end)

let fold_lefti f =
  let i = ref (-1) in
  let f = fun acc elt -> i := !i + 1; f !i acc elt in
  let rec loop acc = function
    | [] -> acc
    | hd :: tl -> loop (f acc hd) tl
  in loop

module Header = struct
  type t = int StringMap.t
  let header_to_map line : t=
    let headers = String.split_on_char '\t' line in
    let map =
      fold_lefti
        (fun i acc title -> StringMap.add title i acc)
        StringMap.empty
        headers
    in
    match StringMap.find_opt "Debut" map with
      None -> failwith "Field 'Debut' must be in the header"
    | Some _ -> map

  let get_elt name header data =
    match StringMap.find_opt name header with
      None -> None
    | Some i -> try Some (data.(i)) with Invalid_argument _ -> None

  let start_year   : t -> string array -> string option = get_elt "Debut"
  let start_month  : t -> string array -> string option = get_elt "Debut mois"
  let end_year     : t -> string array -> string option = get_elt "Fin"
  let end_month    : t -> string array -> string option = get_elt "Fin mois"
  let typ          : t -> string array -> string option = get_elt "Type"
  let typ2         : t -> string array -> string option = get_elt "Type 2"
  let importance   : t -> string array -> string option = get_elt "Ponderation"
  let media        : t -> string array -> string option = get_elt "Media"
  let confidentiel : t -> string array -> string option = get_elt "Confidentiel"
  let title        : t -> string array -> string option = get_elt "Titre"
  let text         : t -> string array -> string option = get_elt "Narration"

  let pp fmt header =
    Format.fprintf fmt
      "%a"
      (Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.fprintf fmt ", ") (fun fmt (str, i) -> Format.fprintf fmt "%s -> %i" str i))
      (List.of_seq (StringMap.to_seq header))

end

let pp_opt pp fmt = function
    None -> ()
  | Some e -> Format.fprintf fmt "%a" pp e

let pp_event fmt (e : event) =
  Format.fprintf fmt
    "{start_date = %a;\n\
     end_date = %a;\n\
     text = %s; %s;\n\
     media = %a;\n\
     group = %a\n\
     confidential = %b\n\
     ponderation = %i}"
    (CalendarLib.Printer.Date.fprint "%D") e.start_date
    (pp_opt (CalendarLib.Printer.Date.fprint "%D")) e.end_date
    e.text.headline e.text.text
    (pp_opt (fun fmt m -> Format.fprintf fmt "%s" m.url)) e.media
    (pp_opt (fun fmt   -> Format.fprintf fmt "%s")) e.group
    e.confidential
    e.ponderation

let pp_title fmt (e : title) =
  Format.fprintf fmt
    "{start_date = %a;\n\
     end_date = %a;\n\
     text = %s; %s;\n\
     media = %a;\n\
     group = %a\n\
     confidential = %b\n\
     ponderation = %i}"
    (pp_opt (CalendarLib.Printer.Date.fprint "%D")) e.start_date
    (pp_opt (CalendarLib.Printer.Date.fprint "%D")) e.end_date
    e.text.headline e.text.text
    (pp_opt (fun fmt m -> Format.fprintf fmt "%s" m.url)) e.media
    (pp_opt (fun fmt   -> Format.fprintf fmt "%s")) e.group
    e.confidential
    e.ponderation

let hd_opt = function
    [] -> None
  | hd :: _ -> Some hd

let fopt f = function
    None -> None
  | Some e -> f e

let list_init n f =
  let rec aux n acc =
    if n < 0 then raise (Invalid_argument "Utils.list_init")
    else if n = 0 then acc
    else aux (n - 1) (f (n - 1) :: acc)
  in
  aux n []
