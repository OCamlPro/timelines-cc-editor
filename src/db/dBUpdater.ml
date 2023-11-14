(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

module V = Db_version
module C = Db_config

let () =
  (* We initialize the database config options. *)
  C.init ();
  (* We update the database. *)
  EzPGUpdater.main
    (C.database ())
    ?host:(C.host ())
    ?port:(C.port ())
    ?user:(C.user ())
    ?password:(C.password ())
    ~upgrades:V.upgrades
    ~downgrades:V.downgrades
