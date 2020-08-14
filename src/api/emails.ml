open Sendgrid_encoding

module StringMap = StringCompat.StringMap

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
  from = email_from_string ?name:!Config.Sendgrid.from_alias !Config.Sendgrid.from;
  subject = Some subject;
  content = Some [{
    content_type = "text/plain";
    content_value = content
  }];
  template_id = None;
  more_fields = None
} 

let creation_email
    ?(lang=En)
    (email : string)
    (timeline_name : string)
    (tid : string) : unit Sendgrid_encoding.mail =
  let url =
    Format.sprintf "%s/edit?timeline=%s-%s"
      !Config.API.api_host
      timeline_name
      tid in
  match lang with
  | En ->
    let subject =
      Format.sprintf
        "Your timeline %s has successfully been created!" timeline_name in
    let content =
      Format.sprintf
        "Dear user,\n\
         Thank you for using timelines.cc! Your timeline is ready to be edited, here is the \n\
         unique link to your timeline: \n\
         %s\n\
         \n\
         If you lose it, you will not have access to your timeline, so make sure you save it \n\
         somewhere. You also can share this link to anyone you want to give edition rights \n\
         to your timeline." url in
    mail_from_email_subject_content email subject content
  | Fr ->
    let subject =
      Format.sprintf
        "Votre frise %s a été créée avec succès !" timeline_name in
    let content =
      Format.sprintf
        "Cher utilisateur,\n\
         Merci d'utiliser timelines.cc ! Votre frise est prête à être utilisée, voici l'unique \n\
         lien vers votre frise :\n\
         %s\n\
         \n\
         Si vous perdez ce lien, vous perdrez l'accès à votre frise donc faites attention à le \n\
         sauvegarder quelque part. vous pouvez aussi partager ce lien aux personnes à qui vous \n\
         donner les droits d'édition à votre frise." url
    in
    mail_from_email_subject_content email subject content
