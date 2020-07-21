type t = (string * string) list

(* Utils *)
let get_args () = Jsloc.args ()
let set_unique key bnd (args : t) = 
  let rec loop acc = function
    | [] -> (key, bnd) :: List.rev acc
    | ((hd_key,_) as hd) :: tl ->
      if hd_key = key then
        (List.rev acc) @ ((key, bnd) :: tl)
      else loop (hd :: acc) tl
  in
  loop [] args

let get_unique = List.assoc_opt
let get_list k =
  List.fold_left
    (fun acc (key, bnd) ->
       if key = k then
         bnd :: acc
       else
         acc
    )
    []

let remove_unique key (args : t) = 
  let rec loop acc = function
    | [] -> args
    | ((hd_key,_) as hd) :: tl ->
      if hd_key = key then
        (List.rev acc) @ tl
      else loop (hd :: acc) tl
  in
  loop [] args 

let set key bnd (args : t) =
  let rec loop acc = function
    | [] -> (key, bnd) :: List.rev acc
    | ((hd_key, hd_bnd) as hd) :: tl ->
      if hd_key = key && hd_bnd = bnd then args (* Argument already present *)
      else loop (hd :: acc) tl
  in
  loop [] args

let remove key bnd (args : t) = 
  let rec loop acc = function
    | [] -> (key, bnd) :: List.rev acc
    | ((hd_key, hd_bnd) as hd) :: tl ->
      if hd_key = key && hd_bnd = bnd then (List.rev acc) @ tl
      else loop (hd :: acc) tl
  in
  loop [] args

let print = 
  Format.pp_print_list
    ~pp_sep:(fun fmt _ -> Format.fprintf fmt "&") 
    (fun fmt (k,b) -> Format.fprintf fmt "%s=%s" k b)

(* Arguments *)
(* Timeline *)
let get_timeline args = List.assoc_opt "timeline" args
let set_timeline tid (args : t) = set_unique "timeline" tid args
let unset_timeline (args : t) = remove_unique "timeline" args

(* Categories *)
let get_categories (args : t) = get_list "group" args
let add_category cat (args : t) = set "group" cat args
let remove_category cat (args : t) = remove "group" cat args

(* Event *)
let get_event args = List.assoc_opt "event" args
let set_event eid (args : t) = set_unique "event" eid args

(* Ponderation *)
let get_min args =
  match List.assoc_opt "min_level" args with
  | None -> None
  | Some i -> try Some (int_of_string i) with _ -> None

let set_min p (args : t) =
  if p = 0 then args else
    set_unique "min_level" (string_of_int p) args

let get_max args = 
  match List.assoc_opt "max_level" args with
  | None -> None
  | Some i -> try Some (int_of_string i) with _ -> None

let set_max p (args : t) =
  if p >= 10000 then args else
  set_unique "max_level" (string_of_int p) args

(* Confidential *)
let get_confidential args =
  match List.assoc_opt "confidential" args with
  | Some "false" | Some "f" -> false
  | _ -> true

let set_confidential b (args : t) =
  set_unique "confidential" (string_of_bool b) args
