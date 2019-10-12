type t

type node = int

val create : int -> t

val add_proposition : t -> string -> node -> t

val add_transition : t -> string -> node -> node -> t

val get_man : t -> MLBDD.man

val get_prop : t -> string -> MLBDD.t

val get_trans : t -> string -> MLBDD.t

val get_tovars_support : t -> MLBDD.support

val to_string : t -> string 