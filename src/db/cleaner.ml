open CalendarLib

let filename : string ref = ref ""
let timelines_to_keep : string list ref = ref []

let dbh () : _ PGOCaml.t PGOCaml.monad =
  let open Config.DB in
  PGOCaml.connect ?host ?password ?port ?user ~database ()

let clean () =    
  let dbh = dbh () in
  let to_remove =
    [%pgsql dbh "SELECT id_, last_update_ FROM timeline_ids_ WHERE id_=alias_"] in
  let today = Date.today () in
  List.iter
    (fun (id, last_update) ->
       match last_update with
       | None -> (* last_update should exist but if none, setting at today *)
         [%pgsql dbh "UPDATE timeline_ids_ SET last_update_=$today WHERE alias_=$id"]
       | Some last_update -> (* if no modification, then discard timeline *)
         let period = Date.sub today last_update in
         if 
           Date.Period.compare period (Date.Period.month 1) >= 0 && 
           not (List.mem id !timelines_to_keep) 
         then begin
           Format.eprintf "Removing timeline %s@." id;
           ignore @@ Database_writer_lib.Writer.remove_timeline id
         end
         else ()
    )
    to_remove

let load_keep () = try
    let l =
      let chan = open_in !filename in
      let str = 
        let res = ref "" in
        let () = 
          try while true do
              res := !res ^ (input_line chan)
            done with End_of_file -> () in
        !res in
      let json = Json_repr.from_yojson @@ Yojson.Safe.from_string str in
      Json_encoding.(destruct (list string) json) 
    in
    List.iter
      (Format.eprintf "Keeping timeline %s@.")
      l;
    timelines_to_keep := l
  with _ -> ()

let () =
  Format.eprintf "Start cleaning@.";
  let () =
    try filename := Sys.argv.(1) with
    _ -> failwith "You jave to provide a list of timelines to keep" in
  while true do
    load_keep ();
    clean ();
    Format.eprintf "Cleaning done, sleeping a day@.";
    Unix.sleep 86400
  done
