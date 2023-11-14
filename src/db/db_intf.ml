(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2020-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU General Public License version 3.0 as described in LICENSE        *)
(*                                                                        *)
(**************************************************************************)

module type MONAD =
sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : exn -> 'a t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  type in_channel
  type out_channel
  val open_connection : Unix.sockaddr -> (in_channel * out_channel) t
  val output_char : out_channel -> char -> unit t
  val output_binary_int : out_channel -> int -> unit t
  val output_string : out_channel -> string -> unit t
  val flush : out_channel -> unit t
  val input_char : in_channel -> char t
  val input_binary_int : in_channel -> int t
  val really_input : in_channel -> Bytes.t -> int -> int -> unit t
  val close_in : in_channel -> unit t

  type 'a pool
  val pool_create : int ->
    ?validate:('a -> bool t) ->
    ?check:('a -> (bool -> unit) -> unit) ->
    ?dispose:('a -> unit t) ->
    (unit -> 'a t) -> 'a pool
  val pool_use : 'a pool -> ('a -> 'b t) -> 'b t
end

module Default_monad = struct
  type 'a t = 'a
  let return x = x
  let (>>=) v f =  f v
  let fail = raise
  let catch f fexn = try f () with e -> fexn e

  type in_channel = Stdlib.in_channel
  type out_channel = Stdlib.out_channel
  let open_connection = Unix.open_connection
  let output_char = output_char
  let output_binary_int = output_binary_int
  let output_string = output_string
  let flush = flush
  let input_char = input_char
  let input_binary_int = input_binary_int
  let really_input = really_input
  let close_in = close_in

  type 'a pool = 'a
  let pool_create _ ?validate ?check ?dispose f =
    let _,_,_ = check,validate,dispose in
    f ()
  let pool_use x f = f x
end


module type READER = sig

  module Monad : MONAD
end

module type READER_GENERIC = functor (M : MONAD) -> READER
  with module Monad = M
   and type 'a Monad.t = 'a M.t

module type WRITER = sig
end

module MakeDBReader(DBG : READER_GENERIC) = functor (M : MONAD) -> struct
  module DB = DBG (M)
  include DB
end

module MakeDBWriter(DB : WRITER) = DB
