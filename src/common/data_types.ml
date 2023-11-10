(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

type date = CalendarLib.Date.t

type text = {
  headline : string;
  text : string
}

type main_event_type = string

type sub_event_type = string

type level = int

type media = {
  url: string;
}

type 'start_date meta_event = {
  start_date: 'start_date;
  end_date: date option;
  text: text;
  media: media option;
  group: main_event_type option;
  confidential: bool;
  ponderation: int;
  unique_id : string;

  (* Not required by timeline *)
  last_update: date option;
  tags : string list
}

type event = date meta_event

type title = date option meta_event

type timeline = {
  events: event list;
  title: title option
}
