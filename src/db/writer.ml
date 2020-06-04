open Data_types
open Utils

let dbh : _ PGOCaml.t PGOCaml.monad =
  let open Config in
  PGOCaml.connect ?host ?password ?port ?user ~database ()

let add_event (e : event) (tid : string) =
  let start_date = e.start_date in
  let end_date = e.end_date in
  let headline = e.text.headline in
  let text = e.text.text in
  let media = opt (fun m -> m.url) e.media in
  let group = e.group in
  let ponderation = Int32.of_int e.ponderation in
  let confidential = e.confidential in
  let unique_id = Utils.check_unique_id Reader.used_unique_id e.unique_id in
  let last_update = e.last_update in
  let tags = List.map (fun s -> Some s) e.tags in
  PGSQL(dbh)
    "INSERT INTO \
     events_(start_date_, end_date_, headline_, text_, \
     media_, group_, confidential_, ponderation_, unique_id_, \
     last_update_, tags_, timeline_id_, is_title_) \
     VALUES($start_date, $?end_date, $headline,$text,\
     $?media,$?group, $confidential, $ponderation, $unique_id, $?last_update, $tags, $tid, false)"


let add_title (t : title) (tid : string) =
  let headline = t.text.headline in
  let text = t.text.text in
  let unique_id = Utils.check_unique_id Reader.used_unique_id t.unique_id in
  match Reader.title tid with
  | None ->
    let () =
      PGSQL(dbh)
        "INSERT INTO events_(headline_, text_, confidential_, ponderation_, timeline_id_, unique_id_, is_title_) \
         VALUES($headline, $text, false, 0, $tid, $unique_id, true)"
    in Ok ()
  | Some _ ->
    Error ("Timeline " ^ tid  ^ "already has a title!")

let update_event (i: int) (e : event) =
    let i = Int32.of_int i in
    let start_date = e.start_date in
    let end_date = e.end_date in
    let headline = e.text.headline in
    let text = e.text.text in
    let media = opt (fun m -> m.url) e.media in
    let group = e.group in
    let ponderation = Int32.of_int e.ponderation in
    let confidential = e.confidential in
    let unique_id = Utils.check_unique_id Reader.used_unique_id e.unique_id in
    let last_update = e.last_update in
    let tags = List.map (fun s -> Some s) e.tags in
    let () =
      PGSQL(dbh) "UPDATE events_ SET start_date_=$start_date, end_date_=$?end_date, \
                  headline_=$headline, text_=$text, media_=$?media, group_=$?group, \
                  confidential_=$confidential, ponderation_=$ponderation, \
                  unique_id_=$unique_id, last_update_=$?last_update, \
                  tags_=$tags WHERE id_=$i" in
    Ok ()

let update_title (i: int) (e : title) =
    let i = Int32.of_int i in
    let start_date = e.start_date in
    let end_date = e.end_date in
    let headline = e.text.headline in
    let text = e.text.text in
    let media = opt (fun m -> m.url) e.media in
    let group = e.group in
    let ponderation = Int32.of_int e.ponderation in
    let confidential = e.confidential in
    let unique_id = Utils.check_unique_id Reader.used_unique_id e.unique_id in
    let last_update = e.last_update in
    let tags = List.map (fun s -> Some s) e.tags in
    let () =
      PGSQL(dbh) "UPDATE events_ SET start_date_=$?start_date, end_date_=$?end_date, \
                  headline_=$headline, text_=$text, media_=$?media, group_=$?group, \
                  confidential_=$confidential, ponderation_=$ponderation, \
                  unique_id_=$unique_id, last_update_=$?last_update, \
                  tags_=$tags WHERE id_=$i" in
    Ok ()

let remove_event (id : int) =
  let id = Int32.of_int id in
  PGSQL(dbh) "DELETE from events_ where id_ = $id"

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

let create_timeline (email : string) (title : title) =
  match Reader.user_exists email with
  | Some _ -> (* User exists, now checking if the timeline already exists *)
    let timeline_id = title.unique_id in
    Format.eprintf "Timeline id before check: %s@." timeline_id;
    let timeline_id = String.map (function ' ' -> '-' | c -> c) timeline_id in
    let timeline_id = Utils.check_unique_id Reader.timeline_exists timeline_id in
    Format.eprintf "Timeline id after check: %s@." timeline_id;
    begin
      try
        match add_title title timeline_id with
        | Ok _ ->
          PGSQL(dbh) "INSERT INTO timeline_ids_(id_) VALUES ($timeline_id)";
          PGSQL(dbh) "UPDATE users_ SET timelines_ = array_append(timelines_, $timeline_id)";
          Ok timeline_id
        | Error e -> Error e
      with e -> Error (Printexc.to_string e)
    end
  | None ->
    Error ("User " ^ email ^ " does not exist")
  
