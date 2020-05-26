
open Data_types
open Db_intf
open Utils

let verbose_mode = ref false
let verbose_counter = ref 0

let search_limit = 20

let hash str = Sha512.(to_hex (string str))

let salted_hash i str = hash ((Int32.to_string i) ^ str)

module Reader_generic (M : Db_intf.MONAD) = struct
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
      PGOCaml.connect ~database:Config.database ())

  let with_dbh f = M.pool_use dbh_pool f

  let (>>>) f g = f g

  let line_to_event ?(with_end_date=true) line =
    match line with
      (id, Some start_date, end_date, headline, text, url, group, confidential, ponderation, unique_id, last_update) ->
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
        last_update
      }

    | _ -> assert false

  let is_auth email cookie =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT id_ FROM users_ WHERE email_=$email" >>=
    function
    | [] -> return false
    | id :: _ ->
      PGSQL(dbh) "SELECT cookie_ FROM sessions_ WHERE user_id_=$id" >>=
      function
      | [] -> return false
      | saved_cookie :: _ ->
      return @@ String.equal cookie saved_cookie

  let get_session user_id =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT cookie_ from sessions_ WHERE user_id_=$user_id" >>=
    function
    | [] -> return None
    | sess :: _ -> return (Some sess)

  let user_exists email =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT id_ FROM users_ WHERE email_=$email" >>=
    function
    | []      -> return None
    | id :: _ -> return (Some id)

  let event auth id =
    let id = Int32.of_int id in
    with_dbh >>> fun dbh ->
      PGSQL(dbh)
      "SELECT id_, start_date_, end_date_, headline_, text_, media_, group_, \
               confidential_, ponderation_, unique_id_, last_update_ FROM events_ \
       WHERE (id_ = $id) AND ($auth OR NOT confidential_)" >>= function
    | res :: _ ->
      let (_, event) = line_to_event res in
      return (Some event)
    | _ -> return None

  let events auth =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT id_, start_date_, end_date_, headline_, text_, media_, group_, \
                confidential_, ponderation_, unique_id_, last_update_ FROM events_ \
                WHERE id_ > 0 AND ($auth OR NOT confidential_) ORDER BY id_ DESC" >>=
    fun l ->
    return @@ List.map line_to_event l

  let title () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT id_, start_date_, end_date_, headline_, text_, media_, group_, \
                confidential_, ponderation_, unique_id_, last_update_ FROM events_ \
                WHERE id_ = 0" >>=
    function
    | [] -> return None
    | (_,_,_,headline, text,_,_,_,_,_,_) :: _ -> return (Some (Utils.to_title_event headline text))

  let category_exists group =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT * FROM groups_ WHERE group_ = $group" >>=
    function
    | [] -> return false
    | _  -> return true

  let categories () =
    with_dbh >>> fun dbh -> PGSQL(dbh) "SELECT * FROM groups_"

  let timeline_data
      ?(start_date      = CalendarLib.Date.mardi_gras 1000)
      ?(end_date        = CalendarLib.Date.mardi_gras 3000)
      ?(min_ponderation = 0)
      ?(max_ponderation = 100)
      ?(groups=[])
      auth
      () =
    let min_ponderation = Int32.of_int min_ponderation in
    let max_ponderation = Int32.of_int max_ponderation in
    with_dbh >>> fun dbh ->
    let req =
      match groups with
        [] -> begin
          PGSQL(dbh)
            "SELECT * FROM events_ WHERE \
             id_ > 0 AND \
             (start_date_ BETWEEN $start_date AND $end_date) AND \
             (ponderation_ BETWEEN $min_ponderation AND $max_ponderation) \
             AND ($auth OR NOT confidential_) \
             ORDER BY id_ DESC" end
      | _ ->
        PGSQL(dbh)
            "SELECT * FROM events_ WHERE \
             id_ > 0 AND \
             group_ IN $@groups AND \
             (start_date_ BETWEEN $start_date AND $end_date) AND \
             (ponderation_ BETWEEN $min_ponderation AND $max_ponderation) \
             AND ($auth OR NOT confidential_) \
             ORDER BY id_ DESC"
    in
    req >>= fun l ->
    return @@ List.map (fun l -> (line_to_event ~with_end_date:true l)) l

  module Login = struct
    let remove_session id =
      with_dbh >>> fun dbh ->
      PGSQL(dbh) "DELETE from sessions_ where user_id_=$id"

    let login email pwdhash =
      with_dbh >>> fun dbh ->
      PGSQL(dbh) "SELECT id_, pwhash_ FROM users_ WHERE email_=$email" >>=
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
            PGSQL(dbh) "INSERT INTO sessions_(user_id_, cookie_) VALUES($id, $cookie)" >>=
            fun _ -> return cookie in
          get_session id >>= (function
              | None -> insert ()
              | Some i -> remove_session id >>= fun _ -> insert ()
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

module Self = Reader_generic(Db_intf.Default_monad)
include Self
