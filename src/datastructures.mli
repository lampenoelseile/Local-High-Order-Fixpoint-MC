open Tcsbasedata
open Tcsset


(* TODO: remove unused methods *)  
(** Type for nodes.
    WARNING: May becomes abstract in the future.
*)
module Node : sig
   type t = 
      SimpleNode of int
   |  NamedNode of string

   val to_string : t -> string
end
(** Type for nodesets. Based on TreeSet.
    @see <https://github.com/tcsprojects/tcslib/blob/master/src/data/tcsset.ml> Treeset of TCSSet
*)
module NodeSet : sig
   type t

   val isEmpty : t -> bool
   (** Checks if the given nodeset is empty.
      @param t set to be checked
      @return if nodeset is empty
   *)

   val compare : t -> t -> int
   
   val elem    : Node.t -> t -> bool
   (** Checks if the given node is a member of the given nodeset.
      @param node node to be checked
      @param t set to check if node is included
      @return if node is included
   *)

   val empty   : t
   (** Constructor for empty nodeset.
      @return empty nodeset
   *)

   val of_node_list    : Node.t list -> t
   (** Creates nodeset out of nodelist.
      @param nodelist list of nodes from which set should be created
      @return set of nodes from node list
   *)                             

   val size    : t -> int
   (** Returns the number of nodes in a nodeset, e.g. the size.
      @param t nodeset which size should be calculated
      @return size of nodeset
   *)
                                                              
   val fold    : ('a -> Node.t -> 'a) -> 'a -> t -> 'a
   (** Fold nodeset. *)                                                        

   val iter    : (Node.t -> unit) -> t -> unit
   (** Iterate nodeset. *)                                               

   val map     : (Node.t -> Node.t) -> t -> t
   (** Map nodeset. *)                                                

   val filter  : (Node.t -> bool) -> t -> t
   (** Filter nodeset via filter function
      
      @param (node -> bool) filter funciton
      @param t nodeset to be filtered
      @retrun filtered nodeset
   *)

   val exists  : (Node.t -> bool) -> t -> bool
   (** Checks if via specified node does exist.
      @param (node -> bool) specifier
      @param t nodeset to be checked
      @return if node with specification exists
   *)                                                

   val forall  : (Node.t -> bool) -> t -> bool
   (** Checks if specification holds for all nodes.
      @param (node -> bool) specifier
      @param t nodeset to be checked
      @return if specification holds for all nodes
   *)                                               

   val find    : (Node.t -> bool) -> t -> Node.t
   (** Finds and returns specified node option.
      @param (node -> bool) specifier
      @param t nodeset to be searched
      @return if found Some node if not None
   *)                                                

   val max     : t -> (Node.t -> Node.t -> bool) -> Node.t
   (** Returns maximum node.
      The maximum is defined via given comparator function.
      @param t nodeset to be searched
      @param (node -> node -> bool) comparator function ( true node one is smaller, false node one is bigger ).
      @return maximum node for comparator
   *)

   val some    : t -> Node.t
   (** Returns a randomly chosen element from a node set.
      
      @param t nodeset to get random node from
      @return random node
   *)

   val first   : t -> Node.t
   (** Returns the smallest (by name) node in a nodeset.
      @param nodeset nodeset to get smallest node from
      @return smallest node by name
   *)

   val last    : t -> Node.t 
   (** Returns greatest (by name) node in a nodeset.
      @param t nodeset to get greatest node from
      @return greatest node by name
   *)
                        
   val add     : Node.t -> t -> t
   (** Add node to nodeset.
      @param node node to add
      @param t nodeset to add to
      @return extended nodeset
   *)

   val del     : Node.t -> t -> t
   (** Delete node from nodeset.
      @param node node to be deleted
      @param t nodeset to remove node from
      @return narrowed nodeset
   *)

   val union   : t -> t -> t
   (** Unifies two nodesets.
      @param t nodeset one to be unified
      @param t nodeset two to be unified
      @return unified nodeset from nodeset one and nodeset two
   *)

   val inter : t -> t -> t
   (** Returns intersection of two nodesets.
       @param t nodeset
       @param t nodeset
       @return intersection of both 
   *)

   val diff : t -> t -> t
   (** Subtracts second nodeset from first one (takes difference)
       @param t nodeset_1 to be subtracted from
       @param t nodeset_2 to subtract
       @return nodeset_1 \ nodeset_2
   *)

   val fold_subsets : (t -> 'a -> 'a) -> t -> 'a -> 'a 
   
   val as_node_list   : t -> Node.t list
   (** Extract a list of nodes from a nodeset.
      @param t nodeset to extract nodes as list from
      @return node list from nodeset 
   *)
   val to_string : t -> string
   (** Returns string representation of nodeset 
      @param t nodeset to be represented
      @return string representation 
   *)
   val init : int -> t
   (** Returns initialized nodeset with number of given int nodes.
      @param nodecount nodes to be added (0... nodecount)
      @return initialized nodeset 
   *)
end

(** Type for set of propositions. Based on TreeSet. Propositions are represented as strings (e.g "p", "q")
    @see <https://github.com/tcsprojects/tcslib/blob/master/src/data/tcsset.ml> Treeset of TCSSet
*)
module PropSet : sig
   (** PropSet type *)
   type t

   val empty : t
   (** Creates empty proposition set
      @return empty PropSet
   *)

   val add : string -> t -> t
   (** Adds proposition to string.
      @param string proposition as string
      @param t propset
      @return updated propset
   *)

   val del : string -> t -> t
   (** Removes proposition from propset.
      @param string proposition as string
      @param t propset
      @return updated propset
   *)

   val of_string_list : string list -> t
   (** Creates propset from string list representation.
      @param stringlist propositions as string list
      @return t propset
   *)

   val as_string_list : t -> string list
   (** Creates string list representation of propset.
      @param t propset
      @return string list propset as string list representation
   *)

   val union : t -> t -> t
   (** Unifies two propsets.
      @param t propset a
      @param t propset b
      @return t union of a and b
   *)

   val to_string : t -> string
   (** Returns string representation of propset.
      @param t propset 
      @return string representation
   *)
end