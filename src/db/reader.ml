
open Timeline_data.Data_types
open Database_interface.Db_intf
open Timeline_data
open DbData
  
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
        media = Utils.opt (fun url -> {url}) url;
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

  (* DO NOT EXPORT: contains the admin key of the timeline *) 
  let filter_of_token (tid : string) =
    with_dbh >>> fun dbh ->
      [%pgsql dbh
        "SELECT alias_, pretty_, min_level_, max_level_, categories_, tags_, \
                confidential_, after_, before_, readonly_ \
         FROM timeline_ids_ WHERE id_=$tid"] >>= function
    | [] -> return (Error "Token does not exist")
    | (Some timeline, pretty, min_level, max_level, categories, tags, 
       confidential_rights, after, before, readonly) :: _ -> 
       let kind = if readonly then View else Edit in 
       let tags = 
         match tags with 
         | None -> None
         | Some p -> Some (
           List.fold_left
             (fun acc -> 
                function
                | None -> acc
                | Some tag -> tag :: acc)
             []
             p) in
       let categories = 
         match categories with 
         | None -> None
         | Some p -> Some (
           List.fold_left
             (fun acc -> 
                function
                | None -> acc
                | Some tag -> tag :: acc)
             []
             p) in
       return (Ok { 
          timeline; pretty; min_level; max_level; categories; tags; 
          confidential_rights; after; before; kind})
    | _ -> assert false

  let unknown_token_error () = Error "Token does not exist"
  
  let with_filter ~error (tid : string) cont =
    filter_of_token tid >>= function
    | Ok f -> cont f
    | Error _ -> error ()

  (* Check confidentiality & filters before using this function. Best not to export it *)
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

(*
  let events (tid : string) =
    with_dbh >>> fun dbh ->
    with_filter ~error:(fun () -> return []) tid 
      (fun {timeline; after; before; min_level; max_level; categories; tags; confidential} ->
        
    [%pgsql dbh "SELECT id_, start_date_, end_date_, headline_, text_, media_, group_, \
                confidential_, ponderation_, unique_id_, last_update_, tags_, is_title_ \
                FROM events_ WHERE \
                timeline_id_ = $timeline AND ($confidential OR NOT confidential_) AND \
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
  )
*)

  (* Do NOT export this function is API: admin check is not performed as
     calling this function is required to check admin in 'update_event'. Also, knowing
     the timeline name from an id could lead to a very simple attack: 
       for i = 0 to max_int do remove_event (timeline_of_event id) id done *)
  let timeline_of_event (id : int) =
    let id = Int32.of_int id in
    with_dbh >>> fun dbh ->
    [%pgsql dbh "SELECT timeline_id_ FROM events_ WHERE id_=$id"] >>=
    function
    | [] -> return None
    | hd :: _ -> return (Some hd)


  let is_admin_filter f tid : bool = f.timeline = tid 

  let admin_rights ~error (timeline_id : string) cont = (* error = If token does not exist *)
    filter_of_token timeline_id >>= (function
        | Ok f ->
          cont (is_admin_filter f timeline_id)
        | Error _ -> return @@ error ())
      
  let has_title (tid : string) =
    with_dbh >>> fun dbh ->
    with_filter ~error:(fun () -> return (Error "Unknown token")) tid (fun f ->
        let admin_tid = f.timeline in
        [%pgsql dbh "SELECT * FROM events_ WHERE timeline_id_ = $admin_tid AND is_title_"]
        >>= function | [] -> return (Ok false) | _ -> return (Ok true)
      )
      
       (* 
  let title (tid : string) =
    with_dbh >>> fun dbh ->
    admin_rights ~error:(fun () -> return None) tid (fun f ->
    [%pgsql dbh
      "SELECT id_, start_date_, end_date_, headline_, text_, media_, group_, \
                confidential_, ponderation_, unique_id_, last_update_, tags_, is_title_ \
       FROM events_ WHERE timeline_id_ = $tid AND is_title_"] >>=
    function
    | [] -> return None
    | l :: _ ->
      let _, id, e = line_to_event l in
      return (Some (id, e))) *)

  let timeline_name (id : string) = 
    with_dbh >>> fun dbh ->
    [%pgsql dbh "SELECT main_title_ FROM timeline_ids_ WHERE id_ = $id"] >>= (function
        | [] -> return (Error "Timeline does not exist")
        | Some hd :: _ -> return (Ok hd)
        | None :: _ -> return (Ok "")) 
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
      ?(min_ponderation = Int32.zero)
      ?(max_ponderation = Int32.max_int)
      ?(groups=[])
      ?(tags=[])
      () =
    with_filter ~error:(fun () -> return (Error "Invalid token")) tid (fun f ->
    let tid = f.timeline in
    let start_date = 
      match f.after with
      | None -> start_date
      | Some d -> Utils.max_date start_date d in
    
    let end_date = 
      match f.before with
      | None -> end_date
      | Some d -> Utils.min_date end_date d in
    
    let min_ponderation = 
      match f.min_level with
      | None -> min_ponderation
      | Some p -> Utils.max32 p min_ponderation in

    let max_ponderation =
      match f.max_level with
      | None -> max_ponderation
      | Some p -> Utils.min32 p max_ponderation in
    
    let tags =
      let tagso =
      match f.tags with
        | None -> tags
        | Some t -> Utils.intersect_list tags t in
      List.map (fun s -> Some s) tagso in

    let groups =
      match f.categories with
      | None -> Format.eprintf "No category filter@."; groups
      | Some c ->
        Format.eprintf "Category filters registered: %a@."(Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.fprintf fmt "//") (fun fmt -> Format.fprintf fmt "%s")) c;
        match groups with
        | [] -> c
        | _ -> Utils.intersect_list groups c in

    let confidential = with_confidential && f.confidential_rights in

    Format.eprintf "Timeline %s with groups = %a@."
      tid
      (Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.fprintf fmt ",") (fun fmt -> Format.fprintf fmt "%s")) groups;
    
    with_dbh >>> fun dbh ->
    let req =
      match groups, tags with
      | [], [] -> begin
          [%pgsql dbh
            "SELECT id_, start_date_, end_date_, headline_, text_, media_, group_, \
                confidential_, ponderation_, unique_id_, last_update_, tags_, is_title_ \
             FROM events_ WHERE \
             ((start_date_ BETWEEN $start_date AND $end_date) OR start_date_ IS NULL) AND \
             (ponderation_ BETWEEN $min_ponderation AND $max_ponderation) AND \
             ($confidential OR NOT confidential_) AND timeline_id_ = $tid \
             ORDER BY id_ ASC"] end
      | _, [] ->
        [%pgsql dbh
            "SELECT id_, start_date_, end_date_, headline_, text_, media_, group_, \
                    confidential_, ponderation_, unique_id_, last_update_, tags_ , is_title_ \
             FROM events_ WHERE \
             group_ IN $@groups AND \
             ((start_date_ BETWEEN $start_date AND $end_date) OR start_date_ IS NULL) AND \
             (ponderation_ BETWEEN $min_ponderation AND $max_ponderation) AND \
             ($confidential OR NOT confidential_) AND timeline_id_ = $tid \
             ORDER BY id_ ASC"]
      | [], _ ->
        [%pgsql dbh
            "SELECT id_, start_date_, end_date_, headline_, text_, media_, group_, \
                confidential_, ponderation_, unique_id_, last_update_, tags_, is_title_ \
             FROM events_ WHERE \
             ((start_date_ BETWEEN $start_date AND $end_date) OR start_date_ IS NULL) AND \
             (ponderation_ BETWEEN $min_ponderation AND $max_ponderation) AND \
             ($confidential OR NOT confidential_) AND timeline_id_ = $tid AND \
             tags_ && $tags::varchar[] ORDER BY id_ ASC"]
      | _ ->
        [%pgsql dbh
            "SELECT id_, start_date_, end_date_, headline_, text_, media_, group_, \
                confidential_, ponderation_, unique_id_, last_update_, tags_, is_title_ \
             FROM events_ WHERE \
             group_ IN $@groups AND \
             ((start_date_ BETWEEN $start_date AND $end_date) OR start_date_ IS NULL) AND \
             (ponderation_ BETWEEN $min_ponderation AND $max_ponderation) AND \
             ($confidential OR NOT confidential_) AND timeline_id_ = $tid AND \
             tags_ && $tags::varchar[] ORDER BY id_ ASC"]
    in
    req >>= (fun l ->
        Format.eprintf "Number of elements: %i@." (List.length l);
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
                match Utils.metaevent_to_event event with
                | Some e  -> (id, e) :: acc
                | None -> acc
            ) [] l
          in return (Ok (!title, events))
        with TwoTitles -> return (Error "Two titles in database")
      ))


  let timeline_exists (tid : string) =
    with_dbh >>> fun dbh ->
    [%pgsql dbh "SELECT * from timeline_ids_ WHERE id_ = $tid"] >>= 
      function 
      | [] -> return false
      | _ -> return true

  (* todo : full sql *)
  let categories (with_confidential : bool) (tid : string) =
    timeline_data ~with_confidential ~tid () >>= (function
    | Ok (title, events) ->
        let s = 
          match title with
          | None | Some (_, {group = None; _}) -> Utils.StringSet.empty
          | Some (_, {group = Some g; _}) -> Utils.StringSet.singleton g in 
        let s =
          List.fold_left
            (fun acc (_, {group; _}) ->
               match group with
               | None -> acc
               | Some g -> Utils.StringSet.add g acc)
            s
            events
        in
        return @@
        Utils.StringSet.fold
          (fun s acc -> s :: acc)
          s
          []
    | _ -> return []
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
(*
  let view
    ?(start_date = CalendarLib.Date.make 1 1 1)
    ?(end_date = CalendarLib.Date.make 3268 1 1)
    ?(min_ponderation = Int32.zero)
    ?(max_ponderation = Int32.max_int)
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
        () *)

  let get_tokens (tid : string) =
    with_dbh >>> fun dbh ->
    admin_rights ~error:unknown_token_error tid (fun has_admin_rights ->
      if has_admin_rights then begin
        [%pgsql dbh
          "SELECT id_, pretty_, min_level_, max_level_, categories_, tags_, \
           confidential_, after_, before_, readonly_ \
           FROM timeline_ids_ WHERE alias_=$tid"] >>=
        fun l ->
        let l = 
          List.map 
            (fun (id, pretty, min_level, max_level, categories, tags, 
                  confidential_rights, after, before, readonly) -> 
              let kind = if readonly then View else Edit in

              let tags = 
                match tags with 
                | None -> None
                | Some p -> Some (
                    List.fold_left
                      (fun acc -> 
                         function
                         | None -> acc
                         | Some tag -> tag :: acc)
                      []
                      p) in

              let categories = 
                match categories with 
                | None -> None
                | Some p -> Some (
                    List.fold_left
                      (fun acc -> 
                         function
                         | None -> acc
                         | Some tag -> tag :: acc)
                      []
                      p) in

              {  (* Exporting a filter with the non admin ID. *)
                timeline = id; pretty; min_level; max_level; categories; 
                tags; confidential_rights; after; before; kind}) l
        in return @@ Ok l
      end else
        return @@ Ok [])

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
