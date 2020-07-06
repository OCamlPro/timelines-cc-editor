open Data_types

let tag_separator = ','

let s = Format.asprintf "\"%s\""

let header =
  ["Start"; "End"; "Headline"; "Text"; "Media";
   "Group"; "Confidential"; "Ponderation"; "Unique Id";
   "Last updated"; "Tags"]

let title_to_csv_line ({
    start_date;
    end_date;
    text;
    media;
    group;
    confidential;
    ponderation;
    unique_id;
    last_update;
    tags
  } : title) : string list = [
    Format.asprintf "%a" (Utils.pp_opt (CalendarLib.Printer.Date.fprint "%Y-%m-%d")) start_date;
    Format.asprintf "%a" (Utils.pp_opt (CalendarLib.Printer.Date.fprint "%Y-%m-%d")) end_date;
    s text.headline;
    s text.text;
    Format.asprintf "%a" (Utils.pp_opt (fun fmt m -> Format.fprintf fmt "%s" m.url)) media;
    Format.asprintf "%a" (Utils.pp_opt (fun fmt -> Format.fprintf fmt "%s")) group;
    string_of_bool confidential;
    string_of_int ponderation;
    s unique_id;
    Format.asprintf "%a" (Utils.pp_opt (CalendarLib.Printer.Date.fprint "%Y-%m-%d")) last_update;
    Format.asprintf "\"%a\"" (Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.fprintf fmt "%c" tag_separator) (fun fmt -> Format.fprintf fmt "%s")) tags
  ]

let csv_line_to_title uids = function
    start_date :: end_date :: headline :: text :: media :: group :: confidential :: ponderation :: unique_id :: rest ->
    let start_date = Utils.string_to_date start_date in
    let end_date = Utils.string_to_date end_date in
    let text = {headline; text} in
    let media = if media = "" then None else Some {url = media} in
    let group = if group = "" then None else Some group in
    let confidential = try bool_of_string confidential with _ -> true in
    let ponderation = try int_of_string ponderation with _ -> 0 in

    let unique_id =
      let id =
      match unique_id with
      | "" -> headline
      | _ -> unique_id in
      Utils.check_unique_id (fun s -> List.mem s uids) id
    in
    let last_update, rest =
      match rest with
        [] -> None, []
      | d :: rest -> Utils.string_to_date d, rest in

    let tags =
      match rest with
      | [] -> []
      | t :: _ -> String.split_on_char tag_separator t in {
    start_date;
    end_date;
    text;
    media;
    group;
    confidential;
    ponderation;
    unique_id;
    last_update;
    tags
  }
  | _ -> failwith "Missing fields"
    
let to_string (c : Csv.t) : string =
  Format.asprintf "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt _ -> Format.fprintf fmt "\n")
       (Format.pp_print_list
          ~pp_sep:(fun fmt _ -> Format.fprintf fmt ",")
          (fun fmt -> Format.fprintf fmt "%s"))) (header :: c)

let from_string (str : string) : title option * event list =
  let chan = Csv.of_string ~separator:',' ~has_header:true ~backslash_escape:true ~excel_tricks:true str in
  let title = try Some (csv_line_to_title [] @@ Csv.next chan) with End_of_file -> None in
  let events =
    let elist = ref [] in
    let uids =
      match title with
      | None -> ref []
      | Some title -> ref [title.unique_id] in
    let exception Not_an_event of string in
    try
      while true do
        let l = Csv.next chan in
        let me = csv_line_to_title !uids l in
        match Utils.metaevent_to_event me with
        | None -> raise (Not_an_event (Format.asprintf "Event %a is incorrect" Utils.pp_title me))
        | Some e ->
          elist := e :: !elist;
          uids  := e.unique_id :: !uids
      done;
      assert false
    with
    | End_of_file -> !elist
  in
  title, events
                       
