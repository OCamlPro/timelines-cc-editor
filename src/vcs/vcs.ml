let delta = 43200 (* Seconds between updates *)

let dir_name = "__backups"

let saves = ref []

let max_saves = 10

let handle_unix = function
  | Unix.WEXITED _ -> ()
  | Unix.WSIGNALED _
  | Unix.WSTOPPED _ ->
    Format.printf "Error on unix command, exiting"; exit 1
  

let create_backup_dir () =
  Format.printf "Creating backup directory@.";
  handle_unix @@ Unix.system ("mkdir -p " ^ dir_name)

let save_db () =
  let file_name =
    Format.asprintf
      "%s/%s-backup-%a-%a.sql"
      dir_name
      Config.DB.database
      (CalendarLib.Printer.Date.fprint "%Y-%m-%d") (CalendarLib.Date.today ())
      (CalendarLib.Printer.Time.fprint "%H-%M-%S") (CalendarLib.Time.now ()) in
  Format.printf "Saving DB on %s@." file_name;
  let () = 
    handle_unix @@
    Unix.system
      (Format.sprintf "pg_dump %s -f %s" Config.DB.database file_name)
  in
  let () =
    if List.length !saves >= max_saves then
      let last, saves_except_last =
        match List.rev !saves with
        | last :: tl -> last, List.rev tl
        | _ -> assert false
      in
      handle_unix @@ Unix.system (Format.sprintf "rm %s" last);
      saves := saves_except_last
  in
  saves := file_name :: !saves
      
     

let () =
  let () = create_backup_dir () in
  while true do
    save_db ();
    Unix.sleep delta      
  done
