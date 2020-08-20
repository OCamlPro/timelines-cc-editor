type kind = View | Edit

type filter = {
  timeline : string; (* Admin ID of the timeline. Must not be exported *)
  kind : kind;       (* Readonly (View) or Edition ? *)
  pretty : string option; (* A pretty identifier *)
  confidential_rights: bool;
 
 (* Each field is optional : None means no filtering *)
  after: CalendarLib.Date.t option;
  before: CalendarLib.Date.t option;
  min_level: int32 option;
  max_level: int32 option;
  categories : string list option;
  tags : string list option;
}

exception TwoTitles

type timeline_data_output = 
  | Timeline of {
      title : (int * Timeline_data.Data_types.title) option;
      events : (int * Timeline_data.Data_types.event) list;
      edition_rights : bool
    }
  | NoTimeline
