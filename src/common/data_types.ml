type date = CalendarLib.Date.t

type text = {
  headline : string;
  text : string
}

(* Format évènement:
   * Debut année
   * Debut mois
   * Fin année
   * Fin mois
   * Type (logiciel, personne, client, + OCaml, blockchain, partenariat/écosystème, etc.)
   * Type 2 (à déterminer)
   * Pondération (0 = confidentiel à masquer)
   * Lien (blogpost, github, lien video etc.)
   * Titre
   * Narration (optionnel)   *)

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
}

type event = date meta_event

type title = date option meta_event

type timeline = {
  events: event list;
  title: title option
}
