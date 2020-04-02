open Data_types

let raw_data = "data"

let full_data = "data.json"

let timeline_data = "data_raw.json"

let link ?(args=[]) path =
  match args with
  | [] -> path
  | _ ->
    let args =
      String.concat "&"
        (List.map (fun (key, value) -> key ^ "=" ^ value) args)
    in
    if String.contains path '?' then
      Printf.sprintf "%s&%s" path args
    else
      Printf.sprintf "%s?%s" path args


let to_int_opt s =
  if s = "" then None
  else Some (int_of_string s)

let to_date year month : date =
  let month =
    match month with
    | None -> 1
    | Some m -> m in
  CalendarLib.Date.make year month 1

let to_date_opt year month =
  match year with
  | None -> None
  | Some year ->
    Some (to_date year month)

let to_text headline text subtyp level =
  let id =
    match subtyp with
      None | Some "" -> ""
    | Some subtyp -> ("id = '" ^ subtyp ^ "'") in
  let cl =
    match level with
      None | Some "" -> ""
    | Some level -> ("class = '" ^ level ^ "'") in
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
