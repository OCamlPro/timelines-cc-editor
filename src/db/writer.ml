open Timeline_data
open Data_types
open Database_reader_lib
open DbData

let dbh : _ PGOCaml.t PGOCaml.monad =
  let open Config.DB in
  PGOCaml.connect ?host ?password ?port ?user ~database ()

let last_update_timeline (tid : string) last_update =
  [%pgsql dbh "UPDATE timeline_ids_ SET last_update_=$?last_update WHERE id_=$tid"]

(* Checks that event can be added/modified with the current filter *)
let event_in_filter (e : title) (f : filter) =
  let ge cmp d1 d2 = 
    match d1,d2 with
      | None, _ | _, None -> true
      | Some d1, Some d2 -> cmp d1 d2 >= 0 in
  ge CalendarLib.Date.compare e.start_date f.after &&
  ge CalendarLib.Date.compare f.before e.end_date &&
  ge Int32.compare (Some (Int32.of_int e.ponderation)) f.min_level &&  
  ge Int32.compare f.max_level (Some (Int32.of_int e.ponderation)) && 

  f.kind = Edit &&
  
  (* If there is a tag limitation, no tags are allowed *)
  begin match f.tags with
    | None -> true
    | Some _ -> Utils.is_empty_list e.tags
  end &&
  
  (* Checking if category is OK *)
  begin
    match e.group, f.categories with 
    | None,_ | Some _, None -> true
    | Some g, Some l -> List.mem g l
  end &&
  
  (* Checking confidentiality: if filter do not allow confidential_rights, 
     then event cannot be confidential *)
  begin
    f.confidential_rights || not (e.confidential) 
  end

let add_event (e : event) (tid : string) =
  match Reader.filter_of_token tid with
  | Ok f ->
    if event_in_filter (Utils.event_to_metaevent e) f then begin
      let full_id = f.timeline in
      let start_date = e.start_date in
      let end_date = e.end_date in
      let headline = e.text.headline in
      let text = e.text.text in
      let media = Utils.opt (fun m -> m.url) e.media in
      let group = e.group in
      let ponderation = Int32.of_int e.ponderation in
      let confidential = e.confidential in
      let unique_id = Utils.check_unique_id Reader.used_unique_id e.unique_id in
      let last_update = e.last_update in
      let tags = List.map (fun s -> Some s) e.tags in
      try
        let () = 
          [%pgsql dbh
              "INSERT INTO \
               events_(start_date_, end_date_, headline_, text_, \
               media_, group_, confidential_, ponderation_, unique_id_, \
               last_update_, tags_, timeline_id_, is_title_) \
               VALUES($start_date, $?end_date, $headline,$text,\
               $?media,$?group, $confidential, $ponderation, $unique_id, $?last_update, $tags, \
               $full_id, false)"] in
        let () = last_update_timeline tid last_update in
        Ok unique_id
      with
        _ -> Error "[Writer.add_event] Error while adding event in DB"
    end
    else
      Error "[Writer.add_event] Incorrect rights"
  | Error e -> Error e

let add_title (t : title) (tid : string) =
  match Reader.filter_of_token tid with
  | Ok f ->
    if event_in_filter t f then begin
      let full_id = f.timeline in
      let headline = t.text.headline in
      let text = t.text.text in
      let unique_id = Utils.check_unique_id Reader.used_unique_id t.unique_id in
      match Reader.has_title tid with
      | Ok false ->
        let () =
          [%pgsql dbh
              "INSERT INTO events_(headline_, text_, confidential_, ponderation_, timeline_id_, \
               unique_id_, is_title_) VALUES($headline, $text, false, 0, $full_id, $unique_id, true)"]
        in
        let () = last_update_timeline tid t.last_update in
        Ok ()
      | Ok true ->
        Error ("Timeline " ^ tid  ^ "already has a title!")
      | Error e -> Error e
    end else 
      Error "[Writer.add_title] Incorrect rights"
  | Error e -> Error e

let update_event (tid : string) (i: int) (e : event) =
  match Reader.timeline_of_event i, Reader.filter_of_token tid with
  | Some full_id, Ok f ->
    if event_in_filter (Utils.event_to_metaevent e) f && full_id = f.timeline then begin
    let i = Int32.of_int i in
    let start_date = e.start_date in
    let end_date = e.end_date in
    let headline = e.text.headline in
    let text = e.text.text in
    let media = Utils.opt (fun m -> m.url) e.media in
    let group = e.group in
    let ponderation = Int32.of_int e.ponderation in
    let confidential = e.confidential in
    let unique_id = Utils.check_unique_id Reader.used_unique_id e.unique_id in
    let last_update = e.last_update in
    let tags = List.map (fun s -> Some s) e.tags in
      let () =
        [%pgsql dbh
            "UPDATE events_ SET start_date_=$start_date, end_date_=$?end_date, \
             headline_=$headline, text_=$text, media_=$?media, group_=$?group, \
             confidential_=$confidential, ponderation_=$ponderation, \
             unique_id_=$unique_id, last_update_=$?last_update, \
             tags_=$tags WHERE id_=$i"] in
      let () = last_update_timeline full_id last_update in
      let () = last_update_timeline tid last_update in
      Ok unique_id
    end else 
      Error "[update_event] Incorrect rights"
  | None, _ -> Error "[update_event] Event is not associated to an existing timeline"
  | _, Error e -> Error e

let update_title (tid : string) (i: int) (e : title) =
  match Reader.timeline_of_event i, Reader.filter_of_token tid with
  | Some full_id, Ok f ->
    if event_in_filter e f && full_id = f.timeline then begin
      let i = Int32.of_int i in
      let start_date = e.start_date in
      let end_date = e.end_date in
      let headline = e.text.headline in
      let text = e.text.text in
      let media = Utils.opt (fun m -> m.url) e.media in
      let group = e.group in
      let ponderation = Int32.of_int e.ponderation in
      let confidential = e.confidential in
      let unique_id = e.unique_id in
      let last_update = e.last_update in
      let tags = List.map (fun s -> Some s) e.tags in
      let () =
        [%pgsql dbh
          "UPDATE events_ SET start_date_=$?start_date, end_date_=$?end_date, \
           headline_=$headline, text_=$text, media_=$?media, group_=$?group, \
           confidential_=$confidential, ponderation_=$ponderation, \
           unique_id_=$unique_id, last_update_=$?last_update, \
           tags_=$tags WHERE id_=$i"] in
      let () = last_update_timeline full_id last_update in
      let () = last_update_timeline tid last_update in
      Ok unique_id
    end else 
      Error "[update_event] Incorrect rights"
  | None, _ -> Error "[update_title] Title is not associated to an existing timeline"
  | _, Error e -> Error e

let remove_event (tid : string) (id : int) =
  match Reader.timeline_of_event id, Reader.filter_of_token tid, Reader.event id with
  | Some full_id, Ok f, Some e ->
    if event_in_filter e f && full_id = f.timeline then begin
      let id = Int32.of_int id in
      let () = [%pgsql dbh "DELETE from events_ where id_ = $id"] in 
      let () = last_update_timeline tid (Some (CalendarLib.Calendar.Date.today ())) in 
      Ok () 
    end else
      Error "[remove_event] Incorrect rights"
  | None, _, _ -> Error "[remove_event] Event is not associated to an existing timeline"
  | _, Error e, _ -> Error e
  | _, _, None -> Error "[remove_event] Event is unknown"

let update_pwd email pwdhash =
  match Reader.user_exists email with
  | None -> Error ("User " ^ email ^ " does not exist")
  | Some i ->
    let real_pwdhash =
      Reader.salted_hash
        i
        pwdhash
    in
    let () = [%pgsql dbh "UPDATE users_ SET pwhash_=$real_pwdhash WHERE email_=$email"] in
    Ok ()

let generate_random_token () =
  let rec loop () =
    let () = Random.self_init () in
    let i1 = Random.int64 Int64.max_int in
    let i2 = Random.int64 Int64.max_int in
    let id = Int64.((to_string i1) ^ to_string i2) in
    if Reader.timeline_exists id then loop () else id in
  loop ()

let create_token
    ?(users=[])
    ?(confidential=false)
    ?(readonly=true)
    ?after
    ?before
    ?min_level
    ?max_level
    ?categories
    ?tags
    ?pretty
    (tid : string) =
  Reader.admin_rights ~error:Reader.unknown_token_error tid (fun admin_rights ->
    if admin_rights then begin
      let new_id = generate_random_token () in
    let users = List.map (fun s -> Some s) users in
    let tags : PGOCaml.string_array option = Utils.opt (List.map (fun s -> Some s)) tags in
    let categories : PGOCaml.string_array option = Utils.opt (List.map (fun s -> Some s)) categories in     
      match Reader.timeline_name tid with
      | Ok timeline_name -> 
        [%pgsql dbh
            "INSERT INTO \
             timeline_ids_(id_, users_, public_, main_title_, alias_, confidential_, readonly_, \
             after_, before_, min_level_, max_level_, categories_, tags_, pretty_) \
             VALUES ($new_id, $users, true, $timeline_name, $tid, $confidential, $readonly, \
             $?after, $?before, $?min_level, $?max_level, $?categories, $?tags, $?pretty)"];
        Ok new_id
      | Error e -> (* Should not happen *) Error e
  end else
    Error "[create_token] Cannot create token")


let raw_remove_timeline tid =
  let () =
    [%pgsql dbh "UPDATE users_ SET timelines_=array_remove(timelines_, $tid)"];
    [%pgsql dbh "DELETE FROM timeline_ids_ WHERE alias_ = $tid"];
    [%pgsql dbh "DELETE FROM events_ WHERE timeline_id_ = $tid"] in
  ()

let remove_timeline tid = 
  Reader.admin_rights ~error:Reader.unknown_token_error tid (fun admin_rights ->
      if admin_rights then begin
        let () = raw_remove_timeline tid in
        Ok ()
      end
      else
        Error "[remove_timeline] Incorrect rights"
    )

let create_private_timeline (email : string) (title : title) (timeline_name : string) =
  match Reader.user_exists email with
  | Some _ -> (* User exists, now checking if the timeline already exists *)
    let timeline_name = String.map (function ' ' -> '-' | c -> c) timeline_name in
    let timeline_id = generate_random_token () in
    let users = [Some email] in
    let today = CalendarLib.Calendar.Date.today () in
    Format.eprintf "Timeline id after check: %s@." timeline_id;
    begin
      try
          [%pgsql dbh
            "INSERT INTO \
             timeline_ids_(id_, users_, public_, main_title_, alias_, \
                           confidential_, readonly_, last_update_, pretty_) \
             VALUES ($timeline_id, $users, false, $timeline_name, $timeline_id, \
                     true, false, $today, 'Admin')";];
          [%pgsql dbh "UPDATE users_ SET timelines_ = array_append(timelines_, $timeline_id) \
                       WHERE email_=$email"];
          match add_title title timeline_id with
          | Ok _ -> begin
            match create_token ~readonly:true ~pretty:"Read only" timeline_id with
            | Ok read_only -> Ok (timeline_id, read_only)
            | Error e -> raw_remove_timeline timeline_id; Error e
          end
          | Error e ->
            Format.eprintf "[create_public_timeline] Error before adding title: %s@." e;
            raw_remove_timeline timeline_id;
            Error e
      with e -> Error (Printexc.to_string e)
    end
  | None -> Error ("User " ^ email ^ " does not exist")  

let create_public_timeline (title : title) (timeline_name : string) =
  let timeline_name = String.map (function ' ' -> '-' | c -> c) timeline_name in
  let timeline_id = generate_random_token () in
  Format.eprintf "[create_public_timeline] Timeline id after check: %s@." timeline_id;
  let users = [] in
  let today = CalendarLib.Calendar.Date.today () in
  Format.eprintf "[create_public_timeline] Creating token@.";
  try
    let () =
      [%pgsql dbh
          "INSERT INTO \
           timeline_ids_(id_, users_, public_, main_title_, alias_, \
           confidential_, readonly_, last_update_, pretty_) \
           VALUES ($timeline_id, $users, true, $timeline_name, $timeline_id, \
           true, false, $today, 'Admin')"] in
    match add_title title timeline_id with
    | Ok _ -> begin
      match create_token ~readonly:true ~pretty:"Read only" timeline_id with
      | Ok read_only -> Ok (timeline_id, read_only)
      | Error e ->
        raw_remove_timeline timeline_id; Error e
        end
    | Error e ->
      raw_remove_timeline timeline_id;
      Format.eprintf "[create_public_timeline] Error before adding title: %s@." e;
      Error e
  with e ->
    let str = Printexc.to_string e in
    Format.eprintf "[create_public_timeline] Error: %s@." str;
    Error (Printexc.to_string e)

let rename_timeline (timeline_id : string) (new_timeline_name : string) =
  Reader.admin_rights ~error:Reader.unknown_token_error timeline_id (fun admin_rights ->
    if admin_rights then
      let () = [%pgsql dbh
        "UPDATE timeline_ids_ SET main_title_=$new_timeline_name WHERE alias_=$timeline_id"]
      in Ok ()
    else Error "[rename_timeline] Incorrect rights"
    )

(* Do NOT export: replace_timeline keeps the old tokens for the new timeline. This
   is a specific behavior for import_timeline *)
let replace_timeline (timeline_id : string) (new_timeline_id : string) =
  Reader.admin_rights ~error:Reader.unknown_token_error timeline_id (fun admin_rights ->
    Reader.admin_rights ~error:Reader.unknown_token_error new_timeline_id (fun admin_rights2 ->
    if admin_rights && admin_rights2 then
      let () = 
        [%pgsql dbh "DELETE FROM events_ WHERE timeline_id_ = $new_timeline_id"] in
      let () =
        [%pgsql dbh "UPDATE events_ SET timeline_id_=$new_timeline_id WHERE timeline_id_=$timeline_id"] in
      let () = raw_remove_timeline timeline_id in
      Ok () 
    else Error "[replace_timeline] Incorrect rights"
  ))
  

let allow_user_to_timeline (email : string) (timeline_id : string) =
  if Reader.timeline_exists timeline_id then
    match Reader.user_exists email with
    | Some _ ->
      let tlist = Reader.user_timelines email in
      if List.mem timeline_id tlist then
        Ok ()
      else
        let () = 
          [%pgsql dbh "UPDATE users_ SET timelines_ = array_append(timelines_, $timeline_id) \
                      WHERE email_=$email"];
          [%pgsql dbh "UPDATE timeline_ids_ SET users_ = array_append(users_, $email) \
                      WHERE id_=$timeline_id"];
        in Ok ()
    | None -> Error "User does not exist!"
  else Error "Timeine does not exist."

let register_user email pwdhash =
  match Reader.user_exists email with
  | Some _ ->
    Error ("User " ^ email ^ " already exists")
  | None -> begin
      let () =
        [%pgsql dbh "INSERT INTO users_(email_, name_, pwhash_) VALUES ($email, $email, '')"]
      in (* Now we get the id of the user *)
      update_pwd email pwdhash
    end

let remove_user email =
  match Reader.user_exists email with
  | Some i -> begin
    let () = Reader.Login.remove_session i in
    let user_timelines = Reader.user_timelines email in
    let () =
      [%pgsql dbh "UPDATE timeline_ids_ SET users_=array_remove(users_, $email)"];
      [%pgsql dbh "DELETE FROM users_ WHERE email_ = $email"] in
    (* If a timeline owned by the deleted account has no more user, it is deleted *)
    let rec remove_unused_timelines = function
    | [] -> Ok ()
    | tid :: tl -> (* Warning: if public timeline are allowed to have users, 
                               check privacy before removing userless timelines *)
      match Reader.timeline_users tid with
      | [] -> begin
        match remove_timeline tid with
        | Ok () -> remove_unused_timelines tl
        | e -> e
      end
      | _ -> remove_unused_timelines tl 
    in 
    remove_unused_timelines user_timelines
    end
  | None -> Error "User does not exist"

let update_token_pretty 
    ~pretty
    ~token
    (tid : string) =
    Reader.admin_rights ~error:Reader.unknown_token_error tid (fun admin_rights ->
      if admin_rights then begin
        [%pgsql dbh
            "UPDATE timeline_ids_ SET pretty_=$?pretty WHERE id_=$token"];
        Ok ()
      end else 
          Error "[update_token] Cannot update token")

let update_token_readonly
    ~readonly
    ~token
    (tid : string) =
    Reader.admin_rights ~error:Reader.unknown_token_error tid (fun admin_rights ->
      if admin_rights then begin
        [%pgsql dbh
            "UPDATE timeline_ids_ SET readonly_=$readonly WHERE id_=$token"];
        Ok ()
      end else 
        Error "[update_token] Cannot update token")

let update_token
    ?(users=[])
    ?(confidential=false)
    ?(readonly=true)
    ?after
    ?before
    ?min_level
    ?max_level
    ?categories
    ?tags
    ?pretty
    ~token
    (tid : string) =
  if token = tid then (* A token cannot change its own rights *)
    Error "[update_token] Cannot self-update a token"
  else
    Reader.admin_rights ~error:Reader.unknown_token_error tid (fun admin_rights ->
        if admin_rights then begin
          let users = List.map (fun s -> Some s) users in
          let tags : PGOCaml.string_array option = Utils.opt (List.map (fun s -> Some s)) tags in
          let categories : PGOCaml.string_array option = Utils.opt (List.map (fun s -> Some s)) categories in 
          let pretty = 
            match pretty with
            | None -> begin
                match Reader.filter_of_token token with 
                | Error _ -> None (* assert false *)
                | Ok f -> f.pretty
              end
            | some -> some in
          [%pgsql dbh
              "UPDATE timeline_ids_ SET \
               users_=$users, confidential_=$confidential, readonly_=$readonly, pretty_=$?pretty, \
               after_=$?after, before_=$?before, min_level_=$?min_level, max_level_=$?max_level, \
               categories_=$?categories, tags_=$?tags WHERE id_=$token"];
          Ok ()
        end
        else
          Error "[update_token] Cannot update token"
      )

let remove_token (tid : string) (token : string) =
  match Reader.filter_of_token tid , Reader.filter_of_token token with
  | Ok f, Ok ftok ->
    if Reader.is_admin_filter f tid && ftok.timeline = tid then begin
      (* Checking if token is tid *)
      if token = tid then Error "[remove_token] Token is admin's: cannot be removed"
      else 
        Ok ([%pgsql dbh "DELETE FROM timeline_ids_ WHERE id_ = $token"])
    end else
      Error "[remove_token] Cannot remove token"
  | (Error _) as e, _ | _, ((Error _) as e) -> e

let update_timeline_name
    new_name
    tid =
      match Reader.filter_of_token tid with
      | Ok {kind = Edit; _} ->
        [%pgsql dbh
            "UPDATE timeline_ids_ SET main_title_=$new_name WHERE alias_=$tid"];
        Ok ()
      | _ -> Error "[update_token] Cannot update timeline name"  
