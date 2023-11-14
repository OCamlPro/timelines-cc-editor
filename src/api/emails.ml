(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

open Sendgrid_encoding

type lang =
  | En
  | Fr

let email_from_string ?name email = {
  email; name
}

let person_from_email email = Sendgrid_encoding.{
    dst = [email_from_string email];
    cc = None;
    bcc = None;
    psubject = None;
    data = None
  }

let mail_from_email_subject_content email subject content = {
  person = [person_from_email email];
  from =
    email_from_string
      ?name:(Api_config.Sendgrid.from_alias ())
      (Api_config.Sendgrid.from ());
  subject = Some subject;
  content = Some [{
    content_type = "text/plain";
    content_value = content
  }];
  template_id = None;
  more_fields = None
}

let creation_email
    ~lang
    ~readonly_tid
    ~admin_tid
    ~email
    ~timeline_name
  : unit Sendgrid_encoding.mail =
  let admin_url =
    Format.sprintf "%s/edit?timeline=%s-%s"
      (Api_config.host ())
      timeline_name
      admin_tid in
  let view_url =
    Format.sprintf "%s/view?timeline=%s-%s"
      (Api_config.host ())
      timeline_name
      readonly_tid in
  match lang with
  | En ->
    let subject =
      Format.sprintf
        "Your timeline %s has successfully been created!" timeline_name in
    let content = (* Todo: html template *)
      Format.sprintf
        "Dear user,\n\
         Thank you for using timelines.cc! Your timeline is ready to be edited, here is the \n\
         unique link to edit your timeline: \n\
         %s\n\
         \n\
         If you lose it, you will not have access to your timeline, so make sure you save it \n\
         somewhere. You also can share this link to anyone you want to give edition rights \n\
         to your timeline.\n\
         \n\
         You also can share your timeline without giving edition rights with the following link:\n\
         %s\n\
         \n\
         We thank you again for using this service. Feel free to reply to this mail if you have\n\
         any question.\n\
         \n\
         Steven, from Timelines.cc" admin_url view_url in
    mail_from_email_subject_content email subject content
  | Fr ->
    let subject =
      Format.sprintf
        "Votre frise %s a été créée avec succès !" timeline_name in
    let content =
      Format.sprintf
        "Cher utilisateur,\n\
         Merci d'utiliser Timelines.cc ! Votre frise est prête à être utilisée, voici l'unique \n\
         lien vers votre frise :\n\
         %s\n\
         \n\
         Si vous perdez ce lien, vous perdrez l'accès à votre frise donc faites attention à le \n\
         sauvegarder quelque part. vous pouvez aussi partager ce lien aux personnes à qui vous \n\
         donner les droits d'édition à votre frise.
         \n\
         Vous pouvez également partager votre frise sans donner les droits d'édition via le lien suivant:\n\
         %s\n\
         \n\
         Nous vous remercions encore une fois d'utiliser nos services. Si vous l'appréciez ou avez des question, vus pouvez répondre à ce mail et je me ferais un plaisir d'y répondre.\n\
         \n\
         Steven, from Timelines.cc" admin_url view_url
    in
    mail_from_email_subject_content email subject content
