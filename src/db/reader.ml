
open Timeline_data.Data_types
open Database_interface.Db_intf
open Timeline_data.Utils

let verbose_mode = ref false
let verbose_counter = ref 0

let search_limit = 20

let hash str = Sha512.(to_hex (string str))

let salted_hash i str = hash ((Int32.to_string i) ^ str)

module Reader_generic (M : MONAD) = struct
  module Monad = M
  open M

  module PGOCaml_old = PGOCaml_generic.Make(M)

  module PGOCaml = struct
    include PGOCaml_old
    let prepare dbh ~name ~query () =
      if !verbose_mode then
        Printf.eprintf "DB %S PREPARE %s\n%!" name query;
      prepare dbh ~name ~query ()

    let execute_rev dbh ~name ~params () =
      if !verbose_mode then begin
        incr verbose_counter;
        let counter = !verbose_counter in
        Printf.eprintf "DB x%dx begin %s\n%!" counter name;
        bind (execute_rev dbh ~name ~params ())
          (fun rows ->
             Printf.eprintf "DB x%dx end %s\n%!" counter name;
             return rows)
      end else
        execute_rev dbh ~name ~params ()
  end
  let dbh_pool =
    let validate conn =
      PGOCaml.alive conn >>= fun is_alive ->
      M.return is_alive in
    let check _conn is_ok =
      is_ok false in
    let dispose conn =
      PGOCaml.close conn in
    M.pool_create ~check ~validate ~dispose 20 (fun () ->
      let open Config.DB in
      PGOCaml.connect ?host ?port ?user ?password ~database ())

  let with_dbh f = M.pool_use dbh_pool f

  let (>>>) f g = f g

  let line_to_event ?(with_end_date=true) line : bool * int * title =
    match line with
      (id, start_date, end_date, headline, text, url, group, confidential,
       ponderation, unique_id, last_update, tags, is_title) ->
      let tags = 
        match tags with 
          | None -> []
          | Some p -> 
            List.fold_left
              (fun acc -> 
                 function
                   | None -> acc
                   | Some tag -> tag :: acc)
              []
              p
      in
      is_title,
      Int32.to_int id, {
        start_date;
        end_date = if with_end_date then end_date else None;
        text = {
          text;
          headline};
        media = opt (fun url -> {url}) url;
        group;
        confidential;
        ponderation = Int32.to_int ponderation;
        unique_id;
        last_update; 
        tags
      }

  let has_admin_rights (email : string) (tid : string) = 
    with_dbh >>> fun dbh ->
    [%pgsql dbh "SELECT timelines_ FROM users_ WHERE email_=$email"] >>=
    fun l ->
    return @@
    List.exists
      (function
        | None -> false
        | Some l -> List.mem (Some tid) l)
      l

  let is_auth (email : string) (cookie : string) =
    with_dbh >>> fun dbh ->
    [%pgsql dbh "SELECT id_ FROM users_ WHERE email_=$email"] >>=
    function
    | [] -> return false
    | id :: _ ->
      [%pgsql dbh "SELECT cookie_ FROM sessions_ WHERE user_id_=$id"] >>=
      function
      | [] -> return false
      | saved_cookie :: _ ->
      return @@ String.equal cookie saved_cookie

  let get_session (user_id : int32) =
    with_dbh >>> fun dbh ->
    [%pgsql dbh "SELECT cookie_ from sessions_ WHERE user_id_=$user_id"] >>=
    function
    | [] -> return None
    | sess :: _ -> return (Some sess)

  let user_exists (email : string) =
    with_dbh >>> fun dbh ->
    [%pgsql dbh "SELECT id_ FROM users_ WHERE email_=$email"] >>=
    function
    | []      -> return None
    | id :: _ -> return (Some id)

  (* Check confidentiality before using this function *)
  let event (id : int) =
    let id = Int32.of_int id in
    with_dbh >>> fun dbh ->
      [%pgsql dbh
      "SELECT id_, start_date_, end_date_, headline_, text_, media_, group_, \
              confidential_, ponderation_, unique_id_, last_update_, tags_, is_title_ FROM events_ \
       WHERE (id_ = $id)"] >>= function
    | res :: _ ->
      let (_, _, event) = line_to_event res in
      return (Some event)
    | _ -> return None

  let events (with_confidential : bool) (tid : string) =
    with_dbh >>> fun dbh ->
    [%pgsql dbh "SELECT id_, start_date_, end_date_, headline_, text_, media_, group_, \
                confidential_, ponderation_, unique_id_, last_update_, tags_, is_title_ \
                FROM events_ WHERE \
                timeline_id_ = $tid AND ($with_confidential OR NOT confidential_) AND \
                NOT is_title_ ORDER BY id_ DESC"] >>=
    fun l ->
    return @@
    List.fold_left (fun acc l ->
        let (_, id, e) = line_to_event l in
        match metaevent_to_event @@ e with
        | None -> acc
        | Some e -> (id, e) :: acc)
      []
      l

  (* Do not export this function is API: admin check is not performed as
     calling this function is required to check admin in 'update_event' *)
  let timeline_of_event (id : int) =
    let id = Int32.of_int id in
    with_dbh >>> fun dbh ->
    [%pgsql dbh "SELECT timeline_id_ FROM events_ WHERE id_=$id"] >>=
    function
    | [] -> return None
    | hd :: _ -> return (Some hd)

  let title (tid : string) =
    with_dbh >>> fun dbh ->
    [%pgsql dbh
      "SELECT id_, start_date_, end_date_, headline_, text_, media_, group_, \
                confidential_, ponderation_, unique_id_, last_update_, tags_, is_title_ from events_ \
       WHERE timeline_id_ = $tid AND is_title_"] >>=
    function
    | [] -> return None
    | l :: _ ->
      let _, id, e = line_to_event l in
      return (Some (id, e))

  let used_unique_id id =
    with_dbh >>> fun dbh ->
    [%pgsql dbh "SELECT unique_id_ FROM events_ WHERE unique_id_ = $id"] >>= (function
        | [] -> return false
        | _ -> return true
      )

  exception TwoTitles

  let timeline_data
      ~(with_confidential : bool)
      ~(tid : string)
      ?(start_date = CalendarLib.Date.make 1 1 1)
      ?(end_date = CalendarLib.Date.make 3268 1 1)
      ?(min_ponderation = 0)
      ?(max_ponderation = 1000)
      ?(groups=[])
      ?(tags=[])
      () =
    let min_ponderation = Int32.of_int min_ponderation in
    let max_ponderation = Int32.of_int max_ponderation in
    let tags = List.map (fun s -> Some s) tags in
    
    with_dbh >>> fun dbh ->
    let req =
      match groups, tags with
      | [], [] -> begin
          [%pgsql dbh
            "SELECT id_, start_date_, end_date_, headline_, text_, media_, group_, \
                confidential_, ponderation_, unique_id_, last_update_, tags_, is_title_ \
             FROM events_ WHERE \
             (start_date_ BETWEEN $start_date AND $end_date) AND \
             (ponderation_ BETWEEN $min_ponderation AND $max_ponderation) AND \
             ($with_confidential OR NOT confidential_) AND timeline_id_ = $tid \
             ORDER BY id_ ASC"] end
      | _, [] ->
        [%pgsql dbh
            "SELECT id_, start_date_, end_date_, headline_, text_, media_, group_, \
                    confidential_, ponderation_, unique_id_, last_update_, tags_ , is_title_ \
             FROM events_ WHERE \
             group_ IN $@groups AND \
             (start_date_ BETWEEN $start_date AND $end_date) AND \
             (ponderation_ BETWEEN $min_ponderation AND $max_ponderation) AND \
             ($with_confidential OR NOT confidential_) AND timeline_id_ = $tid \
             ORDER BY id_ ASC"]
      | [], _ ->
        [%pgsql dbh
            "SELECT id_, start_date_, end_date_, headline_, text_, media_, group_, \
                confidential_, ponderation_, unique_id_, last_update_, tags_, is_title_ \
             FROM events_ WHERE \
             (start_date_ BETWEEN $start_date AND $end_date) AND \
             (ponderation_ BETWEEN $min_ponderation AND $max_ponderation) AND \
             ($with_confidential OR NOT confidential_) AND timeline_id_ = $tid AND \
             tags_ && $tags::varchar[] ORDER BY id_ ASC"]
      | _ ->
        [%pgsql dbh
            "SELECT id_, start_date_, end_date_, headline_, text_, media_, group_, \
                confidential_, ponderation_, unique_id_, last_update_, tags_, is_title_ \
             FROM events_ WHERE \
             group_ IN $@groups AND \
             (start_date_ BETWEEN $start_date AND $end_date) AND \
             (ponderation_ BETWEEN $min_ponderation AND $max_ponderation) AND \
             ($with_confidential OR NOT confidential_) AND timeline_id_ = $tid AND \
             tags_ && $tags::varchar[] ORDER BY id_ ASC"]
    in
    req >>= (fun l ->
        try
          let title = ref None in
          let events =
            List.fold_left (
              fun acc l ->
                let is_title, id, event = line_to_event ~with_end_date:true l in
                if is_title then 
                  match !title with
                  | None -> title := Some (id, event); acc
                  | Some _ -> raise TwoTitles
                else
                match metaevent_to_event event with
                | Some e  -> (id, e) :: acc
                | None -> acc
            ) [] l
          in return (Ok (!title, events))
        with TwoTitles -> return (Error "Two titles in database")
      )


  let timeline_exists (tid : string) =
    with_dbh >>> fun dbh ->
    [%pgsql dbh "SELECT * from timeline_ids_ WHERE id_ = $tid"] >>= 
      function 
      | [] -> return false
      | _ -> return true

  (* todo : full sql *)
  let categories (with_confidential : bool) (tid : string) =
    events with_confidential tid >>= (fun l ->
        let s =
          List.fold_left
            (fun acc (_, {group; _}) ->
               match group with
               | None -> acc
               | Some g -> StringSet.add g acc)
            StringSet.empty
            l
        in
        return @@
        StringSet.fold
          (fun s acc -> s :: acc)
          s
          []
      )

  let user_timelines (email : string) =
    with_dbh >>> fun dbh ->
    [%pgsql dbh "SELECT timelines_ FROM users_ WHERE email_=$email"] >>=
    fun l -> return @@
      List.fold_left
        (fun acc ->
           function
           | None -> acc
           | Some l -> 
             List.fold_left
               (fun acc -> function | None -> acc | Some e -> e :: acc)
               acc
               l
        )
        []
        l

  let timeline_users (tid : string) =
    with_dbh >>> fun dbh ->
    [%pgsql dbh "SELECT users_ FROM timeline_ids_ WHERE id_=$tid"] >>=
    fun l -> return @@
      List.fold_left
        (fun acc ->
           function
           | None -> acc
           | Some l -> 
             List.fold_left
               (fun acc -> function | None -> acc | Some e -> e :: acc)
               acc
               l
        )
        []
        l

  let is_public (tid : string) =
    with_dbh >>> fun dbh ->
    [%pgsql dbh "SELECT public_ FROM timeline_ids_ WHERE id_=$tid"] >>=
      function
      | [] ->     return @@ Error ("Timeline do not exist")
      | b :: _ -> return @@ Ok b

  let view
    ?(start_date = CalendarLib.Date.make 1 1 1)
    ?(end_date = CalendarLib.Date.make 3268 1 1)
    ?(min_ponderation = 0)
    ?(max_ponderation = 100000)
    ?(groups=[])
    ?(tags=[])
    ~(tid : string) =
    with_dbh >>> fun dbh ->
    let tid = Hex.to_string (`Hex tid) in
    [%pgsql dbh "SELECT id_ FROM timeline_ids_ WHERE digest(id_, 'sha256') = $tid"] >>=
    function
    | [] -> return @@ Error ("Timeline do not exist")
    | tid :: _ ->
      timeline_data
        ~with_confidential:false
        ~tid
        ~start_date
        ~end_date
        ~min_ponderation
        ~max_ponderation
        ~groups
        ~tags
        ()

  let get_view_token (tid : string) =
    with_dbh >>> fun dbh ->
    timeline_exists tid >>=
    fun exists ->
    if exists then
      [%pgsql dbh "SELECT digest($tid, 'sha256')"] >>=
      function
      | [] -> assert false
      | Some hsh :: _ -> return @@ Ok (Hex.(show @@ of_string hsh))
      | None :: _ -> return @@ Error "Get view token failed"
    else
      return @@ Error "Timeline do not exist"

  module Login = struct
    let remove_session id =
      with_dbh >>> fun dbh ->
      [%pgsql dbh "DELETE from sessions_ where user_id_=$id"]

    let login email pwdhash =
      with_dbh >>> fun dbh ->
      [%pgsql dbh "SELECT id_, pwhash_ FROM users_ WHERE email_=$email"] >>=
      function
      | [] ->
        return None
      | (id, saved_hash) :: _ ->
        let challenger_hash = salted_hash id pwdhash in
        let res = String.equal challenger_hash saved_hash in
        if res then begin
          Format.printf "New login from %s@." email;
          let insert () =
            let today = CalendarLib.Date.today () in
            let today_str = CalendarLib.Printer.DatePrinter.sprint "%d" today in
            let cookie = hash (today_str ^ challenger_hash) in
            [%pgsql dbh "INSERT INTO sessions_(user_id_, cookie_) VALUES($id, $cookie)"] >>=
            fun _ -> return cookie in
          get_session id >>= (function
              | None -> insert ()
              | Some _sess -> remove_session id >>= fun _ -> insert ()
            ) >>= fun cookie ->
          return (Some cookie)
        end else begin
          Format.printf "Failed login on %s@." email;
          return None
        end

    let logout email cookie =
      is_auth email cookie >>= (fun is_auth ->
          if is_auth then
            user_exists email >>= (function
                | None -> return ()
                | Some id -> remove_session id
              )
          else return ()
        )
  end
end

module Self = Reader_generic(Default_monad)
include Self
