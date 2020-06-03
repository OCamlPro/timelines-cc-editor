open Data_types
open Utils

let dbh : _ PGOCaml.t PGOCaml.monad = PGOCaml.connect ~database:Config.database ()

let add_category str =
  let open Db_intf.Default_monad in
  if not (str = "") then
    Reader.category_exists str >>= (fun group_exists ->
        if not group_exists
        then PGSQL(dbh) "INSERT INTO groups_(group_) VALUES ($str)";
      )

let add_event (e : event) =
  let start_date = e.start_date in
  let end_date = e.end_date in
  let headline = e.text.headline in
  let text = e.text.text in
  let media = opt (fun m -> m.url) e.media in
  let group = e.group in
  let ponderation = Int32.of_int e.ponderation in
  let confidential = e.confidential in
  let unique_id = e.unique_id in
  let last_update = e.last_update in
  let tags = List.map (fun s -> Some s) e.tags in
  let () =
    PGSQL(dbh)
      "INSERT INTO \
       events_(start_date_, end_date_, headline_, text_, \
       media_, group_, confidential_, ponderation_, unique_id_, last_update_, tags_) \
       VALUES($start_date, $?end_date, $headline,$text,\
       $?media,$?group, $confidential, $ponderation, $unique_id, $?last_update, $tags)"
  in
  match group with
  | None -> ()
  | Some group -> add_category group; ()

let add_title (t : title) =
  let headline = t.text.headline in
  let text = t.text.text in
  let () =
    match Reader.title () with
      None -> ()
    | Some _ ->
      PGSQL(dbh) "DELETE FROM events_ where id_ = 0"
  in
  let () =
    PGSQL(dbh)
      "INSERT INTO events_(id_, headline_, text_, confidential_, ponderation_) \
       VALUES(0, $headline, $text, false, 0)"
  in Ok ()

let update_event (i: int) (e : event) =
  if i = 0 then
    Error "Id 0 is reserved for the title"
  else
    let i = Int32.of_int i in
    let start_date = e.start_date in
    let end_date = e.end_date in
    let headline = e.text.headline in
    let text = e.text.text in
    let media = opt (fun m -> m.url) e.media in
    let group = e.group in
    let ponderation = Int32.of_int e.ponderation in
    let confidential = e.confidential in
    let unique_id = e.unique_id in
    let last_update = e.last_update in
    let tags = List.map (fun s -> Some s) e.tags in
    let () = PGSQL(dbh) "UPDATE events_ SET start_date_=$start_date, end_date_=$?end_date, \
                         headline_=$headline, text_=$text, media_=$?media, group_=$?group, \
                         confidential_=$confidential, ponderation_=$ponderation, \
                         unique_id_=$unique_id, last_update_=$?last_update, \
                         tags_=$tags WHERE id_=$i";
      match group with
      | None -> ()
      | Some group -> add_category group in
    Ok ()

let update_title (e : title) =
  let start_date = e.start_date in
  let end_date = e.end_date in
  let headline = e.text.headline in
  let text = e.text.text in
  let media = opt (fun m -> m.url) e.media in
  let group = e.group in
  let ponderation = Int32.of_int e.ponderation in
  let confidential = e.confidential in
  let last_update = e.last_update in
  let tags = List.map (fun s -> Some s) e.tags in
  let () = PGSQL(dbh) "UPDATE events_ SET start_date_=$?start_date, end_date_=$?end_date, \
                       headline_=$headline, text_=$text, media_=$?media, group_=$?group, \
                       confidential_=$confidential, ponderation_=$ponderation, \
                       last_update_=$?last_update, tags_=$tags WHERE id_=0";
    match group with
    | None -> ()
    | Some group -> add_category group in
  Ok ()

let remove_event id =
  let id = Int32.of_int id in
  PGSQL(dbh) "DELETE from events_ where id_ = $id"

let remove_events () =
  PGSQL(dbh) "DELETE from events_";
  PGSQL(dbh) "DELETE from groups_"

let update_pwd email pwdhash =
  match Reader.user_exists email with
  | None -> Error ("User " ^ email ^ " does not exist")
  | Some i ->
    let real_pwdhash =
      Reader.salted_hash
        i
        pwdhash
    in
    let () =
      PGSQL(dbh) "UPDATE users_ SET pwhash_=$real_pwdhash WHERE email_=$email"
    in
    Ok ()

let register_user email pwdhash =
  match Reader.user_exists email with
  | Some _ ->
    Error ("User " ^ email ^ " already exists")
  | None -> begin
      let () =
        PGSQL(dbh) "INSERT INTO users_(email_, name_, pwhash_) VALUES ($email, $email, '')"
      in (* Now we get the id of the user *)
      update_pwd email pwdhash
    end
