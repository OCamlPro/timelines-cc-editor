(* Format: timeline,bool;timeline,bool;... where bool is set for readonly *)

let get_timelines () =
  let tls = List.assoc_opt "timelines" (Cookie.all ()) in
  match tls with
  | None -> []
  | Some str ->
    let pairs = String.split_on_char ';' str in
    List.fold_left
      (fun acc pair ->
         let l = String.split_on_char ',' pair in
         match List.rev l with
         | [] -> assert false
         | [token] -> if token = "" then acc else (token, true) :: acc
         | maybool :: rest -> begin
             try
               let readonly = bool_of_string maybool in
               ((String.concat "," (List.rev rest)), readonly) :: acc
             with _ ->
               (pair, true) :: acc
           end
      )
      []
      pairs

let url token readonly =
  Format.asprintf
    "https://ez-timeline.ocamlpro.com/%s?timeline=%s"
    (if readonly then "view" else "edit")
    token

let add_timeline token readonly =
  let tls = get_timelines () in
  let rec replace_or_add = function
    | [] -> [token, readonly]
    | (tk, ro) :: tl ->
      if tk = token then
        (token, readonly) :: tl
      else
        (tk, ro) :: replace_or_add tl in
  let new_tls = replace_or_add tls in
  let str =
    List.fold_left
      (fun acc (tk, ro) -> acc ^ tk ^ "," ^ string_of_bool ro ^ ";")
      ""
      new_tls in
  Cookie.set "timelines" str

let remove_timeline token =
  let tls = get_timelines () in
  let rec remove = function
    | [] -> []
    | (tk, ro) :: tl ->
      if tk = token then
        tl
      else
        (tk, ro) :: remove tl in
  let new_tls = remove tls in
  let str =
    List.fold_left
      (fun acc (tk, ro) -> acc ^ tk ^ "," ^ string_of_bool ro ^ ";")
      ""
      new_tls in
  Cookie.set "timelines" str
  
