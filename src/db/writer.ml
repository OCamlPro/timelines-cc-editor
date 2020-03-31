open Data_types
open Utils

let dbh : _ PGOCaml.t PGOCaml.monad = PGOCaml.connect ~database:Config.database ()

let add_event (e : event) =
  let start_date = e.start_date in
  let end_date = e.end_date in
  let headline = e.text.headline in
  let text = e.text.text in
  let media = opt (fun m -> m.url) e.media in
  let group = type_to_str e.group in
  PGSQL(dbh) "INSERT INTO events_(start_date_, end_date_, headline_, text_, media_, group_) \
              VALUES($start_date, $?end_date, $headline,$text,$?media,$group)"

let add_title (t : title) =
  let headline = t.headline in
  let text = t.text in
  let () =
    match Reader.title () with
      None -> ()
    | Some _ ->
      PGSQL(dbh) "DELETE FROM events_ where id_ = 0"
  in
  let rows =
    PGSQL(dbh) "INSERT INTO events_(id_, headline_, text_) \
                VALUES(0, $headline, $text)"
  in rows
