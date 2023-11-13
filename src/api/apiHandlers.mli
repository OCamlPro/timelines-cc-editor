(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

type 'a ans =  ('a, string) result EzAPIServerUtils.Answer.t Lwt.t

type ('input, 'res) handler0 =
  EzAPI.Req.t -> EzAPI.Security.basic list -> 'input -> 'res ans

type ('a, 'input, 'res) handler1 =
  (EzAPI.Req.t * 'a) ->
  EzAPI.Security.basic list ->
  'input ->
  'res ans

val is_auth : (unit, bool) handler0

val add_event :
  (Api_data.timeline_id, Data_types.event, Api_data.event_id) handler1

val update_event : (
  Api_data.db_event_id *
  Data_types.title *
  Data_types.title *
  Api_data.timeline_id,
  Data_types.date option Api_data.update_meta_event_res
) handler0

val timeline_data :
  (Api_data.timeline_id, unit, Db_data.timeline_data_output) handler1

val remove_event : (Api_data.timeline_id, unit, unit) handler1

val categories : (Api_data.timeline_id, unit, string list) handler1

val register_user : ((string * string), unit) handler0

val login : ((string * string), string) handler0

val logout : ((string * string), unit) handler0

val create_timeline :
  (string, Data_types.title * bool, string * string) handler1

val import_timeline :
  (string, Data_types.title * Data_types.event list * bool, unit) handler1

val user_timelines : (unit, string list) handler0

val allow_user : (string * string, unit) handler0

val timeline_users : (string, unit, string list) handler1

val remove_user : (unit, unit) handler0

val remove_timeline : (Api_data.timeline_id, unit, unit) handler1

val create_token : (string, unit, string) handler1

val update_token_pretty : (string, string, unit) handler1

val update_token_readonly : (string, string, unit) handler1

val update_token : (string, string, unit) handler1

val remove_token : (string, string, unit) handler1

val get_tokens : (string, unit, Db_data.filter list) handler1

val timeline_name : (Api_data.timeline_id, unit, string) handler1

val update_timeline_name : (string, unit) handler0

val has_admin_rights : (Api_data.timeline_id, unit, bool) handler1

val version : (unit, string) handler0
