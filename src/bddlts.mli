open Tcsset 

type t
(*LTS, represented via bdds (see MLBDD pkg). 

  States are encoded binary.

  Assumed we need n (up to 2^n states) bits, then a proposition is 
  represented as a bdd of at most (maybe its reduced) n variables.
  A transition is represented as a bdd with up to 2*n variables 
  (encoding of from_state and to_state) 
*)

type state = int
(*Publicly, states are accessable via integers*)

val create : int -> t
(** Creates LTS without any transitions or valid propositions.
    @param int lts size
    @return lts with dummy nodes
*)

val add_proposition : t -> string -> state -> t
(** Adds new propsitions to node of given lts.
    @param t LTS   
    @param string proposition to be added
    @param int state to add prop to 
    @return LTS with updated propset of node
*)

val add_transition : t -> string -> state -> state -> t
(** Adds new transition to LTS.
    @param t LTS
    @param string transition
    @param state from
    @param state to
    @return LTS with added transition
*)

val get_man : t -> MLBDD.man
(** Returns BDD manager of this lts (see MLBBD)
    @param t lts
    @return manager 
*)

val get_prop : t -> string -> MLBDD.t
(** Returns proposition / predicate represented as BDD.
    @param t lts
    @param string proposition
    @return proposition represented as bdd
*)

val get_trans : t -> string -> MLBDD.t
(** Returns transition as BDD. 
    @param t lts 
    @param string transition
    @return transition represented as bdd
*)

val get_statescoded : t -> MLBDD.t list 
(** Returns list of (binary) encoding of states in form
    of a bdd list. 
    @param t lts 
    @return encoded states list
*)
val get_allstates : t -> MLBDD.t
(** Returns a bdd representation of all valid states.
    Especially, this excludes states which can be represented
    with n bits, but are not specified in the lts definition.
    @param t lts
    @return all states represented as bdd.
*)

val get_diamond_trans_of : t -> string -> MLBDD.t -> MLBDD.t
(** Returns the interpretation of the modal logic operation
    (<trans> bdd) over the given lts. 
    @param t lts
    @param string transition
    @param MLBDD.t  bdd to apply diamond to 
                    (e.g. some predicate representation)
    @return bdd representation of <trans> (bdd)
*)

val get_box_trans_of : t -> string -> MLBDD.t -> MLBDD.t
(** Returns the interpretation of the modal logic operation
    ([trans] bdd) over the given lts. 
    @param t lts
    @param string transition
    @param MLBDD.t  bdd to apply box to 
                    (e.g. some predicate representation)
    @return bdd representation of [trans] (bdd)
*)

val get_all_sats : t -> MLBDD.t -> state list
(** Returns a list of all satisfying states of some
    bdd for the given lts. 
    @param t lts
    @param MLBDD.t bdd to get sats for
    @return satisfying states
*)
val to_string : t -> string
(** Returns string representation of given lts. 
    @param t lts
    @return string representation
*)