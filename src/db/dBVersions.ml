
(* Some rules:
   * Names that we introduce should end with '_' (it is a standard SQL rule);
   * Use EzPG.Mtimes to add row_created_ and row_modified_ columns in a table;
*)

let default_database = Config.database

let sql_downgrade_1_to_0 = [
  {| DROP TABLE sessions_; |};
  {| DROP TABLE users_; |};
]

let sql_upgrade_0_to_1 =
  [
    {| ALTER ROLE SESSION_USER SET search_path TO db,public|};
    {| CREATE TABLE users_ (
      id_     SERIAL PRIMARY KEY NOT NULL,
      email_  VARCHAR(100) UNIQUE NOT NULL,
      name_   VARCHAR(100) NOT NULL,
      pwhash_ BYTEA NOT NULL
      )|};
    {| CREATE TABLE sessions_ (
      user_id_   integer REFERENCES users_(id_) PRIMARY KEY NOT NULL,
      cookie_    VARCHAR(100) NOT NULL)|};
  ]


let sql_downgrade_2_to_1 = [
  {| DROP TABLE events_ |}
]
let sql_upgrade_1_to_2 = [
  {| CREATE TABLE events_ (
    id_           SERIAL PRIMARY KEY NOT NULL,
    start_date_   DATE,
    end_date_     DATE,
    headline_     TEXT NOT NULL,
    text_         TEXT NOT NULL,
    media_        TEXT,
    group_        VARCHAR(100),
    confidential_ BOOLEAN NOT NULL,
    ponderation_  INT NOT NULL
    )|};
]

let sql_downgrade_3_to_2 = [
  {| DROP TABLE groups_ |}
]

let sql_upgrade_2_to_3 =  [
  {| CREATE TABLE groups_ (
     group_ VARCHAR(100) PRIMARY KEY NOT NULL
  )|}
]

let ( upgrades, downgrades ) =
  let rev_versions = ref [] in
  let versions = List.mapi (fun i (upgrade, downgrade) ->
      rev_versions := (i+1, downgrade) :: !rev_versions;
      i,
      fun (dbh : unit PGOCaml.t) version ->
        EzPG.upgrade ~dbh ~version ~downgrade upgrade)
      [
        sql_upgrade_0_to_1, sql_downgrade_1_to_0;
        sql_upgrade_1_to_2, sql_downgrade_2_to_1;
        sql_upgrade_2_to_3, sql_downgrade_3_to_2;
      ]
  in
  (versions, !rev_versions)
