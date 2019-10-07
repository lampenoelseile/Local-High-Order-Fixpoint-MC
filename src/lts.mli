open Datastructures
(** Simple LTS with just one kind of transition and without specified initial states*)

(** LTS type *)
type t

val create_empty : t 
(** Creates empty LTS.
    @return empty lts
*)
val create_dummy  : int -> t
(** Creates LTS without any transitions or valid propositions.
    @param int lts size
    @return lts with dummy nodes
*)

val create_random : int -> string list -> string list -> float -> float -> t
(** Creates random LTS.
    @param int amount of nodes
    @param string list available transitions
    @param string list available propositions
    @param float percentage of edges to be set randomly
    @param float percentage of propositions to be set randomly
    @return randomly-generated lts
*)
val create_dummy_from_nodeset : NodeSet.t -> t
(** Creates LTS without any transitions or valid propositions,
    but specified nodes.
    @param nodeset nodes to create lts from
    @return dummy lts with nodes from nodeset
*)

val add_node : t -> Node.t -> t
(** Adds new node to LTS.
    @param t LTS
    @param node node to add
    @return LTS with added node
*)

val add_transition : t -> string -> Node.t -> Node.t -> t
(** Adds new edge to LTS.
    @param t LTS
    @param string transition
    @param node node where edge starts
    @param node node where edge ends
    @return LTS with added edge
*)

val add_proposition : t -> Node.t -> string -> t
(** Adds new propsitions to node of given lts.
    @param t LTS   
    @param node 
    @param string proposition to be added
    @return LTS with updated propset of node
*)

val get_all_nodes : t -> NodeSet.t
(**  Returns complete nodeset of lts.
     @param t LTS
     @return nodeset
*)
val get_trans_predecessors_of_node : t -> string -> Node.t -> NodeSet.t
(** Returns predecessors of given node.
    @param t LTS
    @param string transition
    @param node 
    @return predecessors nodeset
*)

val get_trans_successors_of_node : t -> string -> Node.t -> NodeSet.t
(** Returns successors of given node.
    @param t LTS
    @param string transition
    @param node 
    @return successors nodeset
*)

val get_nodes_of_proposition : t -> string -> NodeSet.t
(** Returns nodes of lts where given proposition is valid.
    @param t LTS
    @param string proposition
    @return nodeset where proposition is valid
 *)

val to_string : t -> string
(** Makes console output of current LTS.
    @param t lts to be printed
 *)

val make_simple : t -> t