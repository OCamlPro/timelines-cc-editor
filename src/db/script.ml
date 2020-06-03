open Data_types

let () =
  let events = Reader.events true in
  let () =
    List.iter
      (fun (id, event) ->
         let event =
           let unique_id =
             match event.text.headline with
             | "" -> "id-" ^ string_of_int id
             | t -> Utils.short_title t in
           {event with unique_id} in
         match Writer.update_event id event with
         | Ok () -> ()
         | Error s ->
           Format.printf "Failing at event %i" id;
           failwith s
      )
      events
  in ()
