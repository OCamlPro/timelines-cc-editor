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

  let start_year  : t -> string array -> string option = get_elt "Debut"
  let start_month : t -> string array -> string option = get_elt "Debut mois"
  let end_year    : t -> string array -> string option = get_elt "Fin"
  let end_month   : t -> string array -> string option = get_elt "Fin mois"
  let typ         : t -> string array -> string option = get_elt "Type"
  let typ2        : t -> string array -> string option = get_elt "Type 2"
  let importance  : t -> string array -> string option = get_elt "Ponderation"
  let media       : t -> string array -> string option = get_elt "Media"
  let title       : t -> string array -> string option = get_elt "Titre"
  let text        : t -> string array -> string option = get_elt "Narration"

  let pp fmt header =
    Format.fprintf fmt
      "%a"
      (Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.fprintf fmt ", ") (fun fmt (str, i) -> Format.fprintf fmt "%s -> %i" str i))
      (List.of_seq (StringMap.to_seq header))

end

let pp_event fmt (e : event) =
  Format.fprintf fmt "{Event '%s' starting on %a}" e.text.headline (CalendarLib.Printer.Date.fprint "%D") e.start_date
