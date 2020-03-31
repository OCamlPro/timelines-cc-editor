type date = CalendarLib.Date.t

type text = {
  headline : string;
  text : string
}

(* Format évènement:
   * Debut
   * Debut mois
   * Fin (optionnel)
   * Fin mois (optionnel)
   * Type (logiciel, personne, client)
   * Type 2 (à déterminer)
   * Pondération (important ou pas)
   * Image ou lien video ou github
   * Titre
   * Narration (optionnel)   *)

type main_event_type = | Software | Person | Client

type sub_event_type = string
type level = int

type media = {
  url: string;
}

type event = {
  start_date: date;
  end_date: date option;
  text: text;
  media: media option;
  group: main_event_type option;
}

type title = text (* An event in Timeline.js, we only use the text here *)

type timeline = {
  events: event list;
  title: title
}
