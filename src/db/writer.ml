open Data_types
open Utils

let dbh : _ PGOCaml.t PGOCaml.monad = PGOCaml.connect ~database:Config.database ()

let add_category str =
  let open Db_intf.Default_monad in
  if not (str = "") then
    Reader.category_exists str >>= (fun group_exists ->
        if not group_exists
        then PGSQL(dbh) "INSERT INTO groups_(group_) VALUES ($str)"
      )

let add_event (e : event) =
  let start_date = e.start_date in
  let end_date = e.end_date in
  let headline = e.text.headline in
  let text = e.text.text in
  let media = opt (fun m -> m.url) e.media in
  let group = e.group in

  let () =
      PGSQL(dbh) "INSERT INTO events_(start_date_, end_date_, headline_, text_, media_, group_) \
                  VALUES($start_date, $?end_date, $headline,$text,$?media,$?group)" in
  match group with
  | None -> ()
  | Some group -> add_category group

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

let update_event (i: int) (e : event) =
  if i = 0 then false
  else
    let i = Int32.of_int i in
    let start_date = e.start_date in
    let end_date = e.end_date in
    let headline = e.text.headline in
    let text = e.text.text in
    let media = opt (fun m -> m.url) e.media in
    let group = e.group in
    let () = PGSQL(dbh) "UPDATE events_ SET start_date_=$start_date, end_date_=$?end_date, \
                headline_=$headline, text_=$text, media_=$?media, group_=$?group WHERE id_=$i";
      match group with
      | None -> ()
      | Some group -> add_category group in
    true

let remove_event id =
  let id = Int32.of_int id in
  PGSQL(dbh) "DELETE from events_ where id_ = $id"
