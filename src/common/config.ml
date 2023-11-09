open Json_encoding

module DB = struct
  let database = "ocptl_db"
  let host = None
  let port = None
  let user = None
  let password = None
end

module API = struct
  let api_port = ref 13579
  let api_host = ref "timelines.cc"
  let api_root = ref (Some "api")

  let encoding =
    obj3
      (opt "api_port" int)
      (opt "api_host" string)
      (opt "api_root" string)

  let init file =
    try
      let ic = open_in file in
      let json = Ezjsonm.from_channel ic in
      close_in ic;
      let api_port', api_host', api_root' = destruct encoding json in
      let (=:=) r v =
        match v with None -> () | Some v -> r := v in
      api_port =:= api_port';
      api_host =:= api_host';
      api_root := api_root'
    with
    | exn ->
      Printf.eprintf "Fatal error while reading %S:\n  %s\n%!"
        file (Printexc.to_string exn);
      exit 2
end

module Sendgrid = struct
  let key : string ref = ref ""
  let from : string ref = ref ""

  let from_alias : string option ref = ref None

  let encoding =
    obj3
      (req "key" string)
      (req "from" string)
      (opt "from_alias" string)

  let init file =
    try
      Format.printf "Reading sendgrid config file %s@." file;
      let ic = open_in file in
      let json = Ezjsonm.from_channel ic in
      close_in ic;
      let key', from', from_alias' = destruct encoding json in
      key := key';
      from := from';
      from_alias := from_alias'
    with
    | exn ->
      Printf.eprintf "Fatal error while reading %S:\n  %s\n%!"
        file (Printexc.to_string exn);
      exit 2
end
