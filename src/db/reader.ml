
open Data_types
open Db_intf

let verbose_mode = ref false
let verbose_counter = ref 0

let search_limit = 20

module Reader_generic (M : Db_intf.MONAD) = struct
  module Monad = M
  open M

  module PGOCaml_old = PGOCaml_generic.Make(M)

  module PGOCaml = struct
    include PGOCaml_old
    let prepare dbh ~name ~query () =
      if !verbose_mode then
        Printf.eprintf "DB %S PREPARE %s\n%!" name query;
      prepare dbh ~name ~query ()

    let execute_rev dbh ~name ~params () =
      if !verbose_mode then begin
        incr verbose_counter;
        let counter = !verbose_counter in
        Printf.eprintf "DB x%dx begin %s\n%!" counter name;
        bind (execute_rev dbh ~name ~params ())
          (fun rows ->
             Printf.eprintf "DB x%dx end %s\n%!" counter name;
             return rows)
      end else
        execute_rev dbh ~name ~params ()
  end
  let dbh_pool =
    let validate conn =
      PGOCaml.alive conn >>= fun is_alive ->
      M.return is_alive in
    let check _conn is_ok =
      is_ok false in
    let dispose conn =
      PGOCaml.close conn in
    M.pool_create ~check ~validate ~dispose 20 (fun () ->
      PGOCaml.connect ~database:Config.database ())

  let with_dbh f = M.pool_use dbh_pool f

  let (>>>) f g = f g
(*
  let of_count_pair =
    let opt_check = function
        Some count -> Int64.to_int count
      | None -> 0 in
    function
    | [ a,b ] -> return (opt_check a, opt_check b)
    | _ -> return (0,0)

  let of_dbf f = function
    | [ c ] -> return (f c)
    | _ -> return (f 0L)
  let of_db_optf f = function
    | [ Some c ] -> return (f c)
    | _ -> return (f 0L)
  let of_db = of_dbf (fun x -> x)
  let of_db_opt = of_db_optf (fun x -> x)
  let of_count_opt = of_db_optf Int64.to_int
  let of_count = of_dbf Int64.to_int *)

  let event id =
    let id = Int32.of_int id in
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT * FROM events_ \
       WHERE (id_ = $id)" >>= function
    | [] -> return None
    | h :: _ -> return (Some h)

  let events () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT * FROM events_"
end
