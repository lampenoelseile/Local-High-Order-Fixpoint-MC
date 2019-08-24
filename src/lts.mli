open Datastructures
(** Simple LTS with just one kind of transition and without specified initial states*)

(** LTS type *)
type t

val create_dummy  : int -> t
(** Creates LTS without any nodes or valid propositions.
    @param int lts size
    @return plain lts
*)

val create_random : int -> string list -> float -> float -> t
(** Creates random LTS.
    @param int amount of nodes
    @param string list available propositions
    @param float percentage of edges to be set randomly
    @param float percentage of propositions to be set randomly
    @return randomly-generated lts
*)

val add_edge : t -> node -> node -> t
(** Adds new edge to LTS.
    @param t LTS
    @param node node where edge starts
    @param node node where edge ends
    @return LTS with added edge
*)
val add_propositions : t -> node -> PropSet.t -> t
(** Adds new propsitions to node of given lts.
    @param t LTS
    @param node 
    @param PropSet.t propositions to be added
    @return LTS with updated propset of node
*)
val get_predecessors_of_node : t -> node -> NodeSet.t
(** Returns predecessors of given node.
    @param t LTS
    @param node 
    @return predecessors nodeset
*)

val get_successors_of_node : t -> node -> NodeSet.t
(** Returns successors of given node.
    @param t LTS
    @param node 
    @return successors nodeset
*)

val get_propositions_of_node : t -> node -> PropSet.t
(** Returns propositions of given node.
    @param t LTS
    @param node 
    @return predecessors nodeset
*)

val get_nodes_of_propositions : t -> PropSet.t -> NodeSet.t
(** CURRENTLY NOT IMPLEMENTED *)

val print : t -> unit
(** Makes console output of current LTS.
    @param t lts to be printed
 *)