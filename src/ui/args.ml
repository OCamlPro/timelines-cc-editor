type t = (string * string) list

let get () = Jsloc.args ()
let set = Jsloc.set_args
let set_unique key bnd (args : t) = 
  let rec loop acc = function
    | [] -> (key, bnd) :: List.rev acc
    | ((hd_key,_) as hd) :: tl ->
      if hd_key = key then
        (List.rev acc) @ ((key, bnd) :: tl)
      else loop (hd :: acc) tl
  in
  loop [] args

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
    
let get_timeline args = List.assoc_opt "timeline" args
let set_timeline tid (args : t) = set_unique "timeline" tid args
let unset_timeline tid (args : t) = remove_unique "timeline" args

let get_categories (args : t) =
  List.fold_left
    (fun acc (key, bnd) ->
       if key = "group" then
         bnd :: acc
       else
         acc
    )
    []
    args

let add_category cat (args : t) = set "group" cat args
let remove_category cat (args : t) = remove "group" cat args
