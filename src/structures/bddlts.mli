open Tcsset 

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

(*module HO : sig
  type base = MLBDD.t

  type map = 
    MapBase of base
  | MapFun of (map,map) TreeMap.t

  type t =
    Base of base
  | Map of map
  | Ref of (t -> t)

  val get_value : t -> t list -> t

  val of_bdd : MLBDD.t -> t

  val to_bdd : t -> MLBDD.t
end*)