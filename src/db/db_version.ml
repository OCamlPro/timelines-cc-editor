(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)


(* Some rules:
   * Names that we introduce should end with '_' (it is a standard SQL rule);
   * Use EzPG.Mtimes to add row_created_ and row_modified_ columns in a table;
*)

let default_database = Db_config.database

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
      cookie_    VARCHAR NOT NULL)|};
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

let sql_downgrade_4_to_3 = [
  {| ALTER TABLE events_ DROP COLUMN unique_id_ |}
]

let sql_upgrade_3_to_4 =  [
  {| ALTER TABLE events_ ADD COLUMN unique_id_ VARCHAR UNIQUE NOT NULL |}
]

let sql_downgrade_5_to_4 = [
  {| ALTER TABLE events_ DROP COLUMN last_update_ |}
]

let sql_upgrade_4_to_5 =  [
  {| ALTER TABLE events_ ADD COLUMN last_update_ DATE |}
]

let sql_downgrade_6_to_5 = [
  {| ALTER TABLE events_ DROP COLUMN tags_ |}
]

let sql_upgrade_5_to_6 =  [
  {| ALTER TABLE events_ ADD COLUMN tags_ VARCHAR[] |}
]

let sql_downgrade_7_to_6 = [
  {| ALTER TABLE events_ DROP COLUMN timeline_id_ |};
  {| ALTER TABLE events_ DROP COLUMN is_title_ |};
  {| ALTER TABLE users_  DROP COLUMN timelines_   |};
  {| DROP TABLE timeline_ids_ |};
  {| CREATE TABLE groups_ (
     group_ VARCHAR(100) PRIMARY KEY NOT NULL
  )|};
]

let sql_upgrade_6_to_7 =  [
  {| ALTER TABLE events_ ADD COLUMN timeline_id_ VARCHAR NOT NULL |};
  {| ALTER TABLE events_ ADD COLUMN is_title_ BOOLEAN NOT NULL |};
  {| ALTER TABLE users_  ADD COLUMN timelines_ VARCHAR[] |};
  {| CREATE TABLE timeline_ids_ (
     id_ VARCHAR PRIMARY KEY NOT NULL,
     users_ VARCHAR[]
  )|};
  {| DROP TABLE groups_ |};
]

let sql_downgrade_8_to_7 = [
  {| ALTER TABLE timeline_ids_ DROP COLUMN public_ |};
  {| ALTER TABLE timeline_ids_ DROP COLUMN last_update_ |};
]

let sql_upgrade_7_to_8 = [
  {| ALTER TABLE timeline_ids_ ADD COLUMN public_ BOOLEAN NOT NULL DEFAULT FALSE|};
  {| ALTER TABLE timeline_ids_ ADD COLUMN last_update_ DATE|};
  {| CREATE EXTENSION pgcrypto |}; (* For allowing hash functions on columns *)
]

let sql_downgrade_9_to_8 = [
  {| ALTER TABLE timeline_ids_ DROP COLUMN main_title_ |};
]

let sql_upgrade_8_to_9 = [
  {| ALTER TABLE timeline_ids_ ADD COLUMN main_title_ VARCHAR|};
]

let sql_downgrade_10_to_9 = [
  {| ALTER TABLE timeline_ids_ DROP COLUMN alias_ |};
  {| ALTER TABLE timeline_ids_ DROP COLUMN readonly_ |};
  {| ALTER TABLE timeline_ids_ DROP COLUMN pretty_ |};
  {| ALTER TABLE timeline_ids_ DROP COLUMN min_level_ |};
  {| ALTER TABLE timeline_ids_ DROP COLUMN max_level_ |};
  {| ALTER TABLE timeline_ids_ DROP COLUMN categories_ |};
  {| ALTER TABLE timeline_ids_ DROP COLUMN tags_ |};
  {| ALTER TABLE timeline_ids_ DROP COLUMN confidential_ |};
  {| ALTER TABLE timeline_ids_ DROP COLUMN after_ |};
  {| ALTER TABLE timeline_ids_ DROP COLUMN before_ |};
]

let sql_upgrade_9_to_10 = [
  {| ALTER TABLE timeline_ids_ ADD COLUMN alias_ VARCHAR REFERENCES timeline_ids_(id_) |};
  {| ALTER TABLE timeline_ids_ ADD COLUMN readonly_ BOOLEAN NOT NULL DEFAULT TRUE |};
  {| ALTER TABLE timeline_ids_ ADD COLUMN pretty_ VARCHAR |};
  {| ALTER TABLE timeline_ids_ ADD COLUMN max_level_ INT |};
  {| ALTER TABLE timeline_ids_ ADD COLUMN min_level_ INT |};
  {| ALTER TABLE timeline_ids_ ADD COLUMN categories_ VARCHAR[] |};
  {| ALTER TABLE timeline_ids_ ADD COLUMN tags_ VARCHAR[] |};
  {| ALTER TABLE timeline_ids_ ADD COLUMN confidential_ BOOLEAN NOT NULL DEFAULT FALSE |};
  {| ALTER TABLE timeline_ids_ ADD COLUMN after_ DATE |};
  {| ALTER TABLE timeline_ids_ ADD COLUMN before_ DATE |};
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
        sql_upgrade_3_to_4, sql_downgrade_4_to_3;
        sql_upgrade_4_to_5, sql_downgrade_5_to_4;
        sql_upgrade_5_to_6, sql_downgrade_6_to_5;
        sql_upgrade_6_to_7, sql_downgrade_7_to_6;
        sql_upgrade_7_to_8, sql_downgrade_8_to_7;
        sql_upgrade_8_to_9, sql_downgrade_9_to_8;
        sql_upgrade_9_to_10, sql_downgrade_10_to_9;
      ]
  in
  (versions, !rev_versions)
