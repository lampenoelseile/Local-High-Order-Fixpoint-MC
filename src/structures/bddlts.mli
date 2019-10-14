open Tcsset 
open Hfl

type t

type state = int

val create : int -> t

val add_proposition : t -> string -> state -> t

val add_transition : t -> string -> state -> state -> t

val get_man : t -> MLBDD.man

val get_prop : t -> string -> MLBDD.t

val get_trans : t -> string -> MLBDD.t

val get_tovars_support : t -> MLBDD.support

val get_tovars_ids_as_array : t -> MLBDD.var array

val get_all_sat_states : t -> MLBDD.t -> state list

val does_satisfy : t -> MLBDD.t -> state -> bool

val to_string : t -> string 

module HO : sig
  type hot =
    Base of MLBDD.t
  | Fun of (hot,hot) TreeMap.t

  val compare : hot -> hot -> int
  val empty_base : t -> hot
  val empty_fun : hot

  val to_string : ?max_length:int -> hot -> string

  val is_defined_for_args : hot -> hot list -> bool

  val get_value_for_args : hot -> hot list -> hot
  
  val set_value_for_args : hot -> hot -> hot list -> hot

  val get_defined_arguments : hot -> (hot list) list

  val all_of_type : t -> Formula.variable_t -> hot list
end