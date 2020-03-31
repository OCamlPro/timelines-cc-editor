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

let opt f = function
  | None -> None
  | Some e -> Some (f e)
