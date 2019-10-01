(* For high-level info about public functions see lts.mli *)
open Tcsbasedata
open Tcsset
open Datastructures


type transkey =
(* type to distinguish if transition map contains e.g. p pred or p succ *)
  | TransIn of string
  | TransOut of string

module TransitionMap = struct
(* transition map. key: transkey a (in or out) value: NodeMap (see below).
   used to get a-pred or a-succ of some node. 
*)

  module NodeMap = struct
  (* node map. key: int/node value: set of nodes. 
     here used as value in transition map some node (key) to some kind of predecessors or successors (value).
  *)
    type t = (Node.t, NodeSet.t) TreeMap.t

    let empty = TreeMap.empty compare
    
    let add_node_to_value nm key_node node_to_add =
      match TreeMap.find_opt key_node nm with
      | None -> TreeMap.add key_node (NodeSet.add node_to_add NodeSet.empty) nm
      | Some(value) -> TreeMap.add key_node (NodeSet.add node_to_add value) nm      
  end

  type t = (transkey, NodeMap.t) TreeMap.t

  let empty = TreeMap.empty compare

  let get_tpredecessors_of_node tm trans node =
  (* returns predecessors of some node for transition trans. 
   returns emptyset if key not included
  *)
    match TreeMap.find_opt node (TreeMap.find (TransIn trans) tm) with
    | None -> NodeSet.empty
    | Some(value) -> value
  
  let get_tsuccessors_of_node tm trans node =
  (* same as for tpredecessors.*)
    match TreeMap.find_opt node (TreeMap.find (TransOut trans) tm) with
    | None -> NodeSet.empty
    | Some(value) -> value
  
  let get_keys_as_list tm =
    let l = ref [] in
    TreeMap.iter (fun k v -> 
                    match k with 
                    | TransIn(s) -> l := s :: !l
                    | _ -> ()) tm;
    !l
  
  let init_transition tm trans =
    (*initializes transition trans in map tm. makes setting transition well-defined.
      if exists tm is simply returned 
    *)
    match (TreeMap.find_opt (TransIn trans) tm, TreeMap.find_opt (TransOut trans) tm) with
    | (None, None) -> TreeMap.add (TransIn trans) (NodeMap.empty) (TreeMap.add (TransOut trans) (NodeMap.empty) tm)
    | (Some (t_in), Some(t_out)) -> tm
    | _ -> print_endline "Init Transmap error. Tm was returned"; tm

  let set_transition tm t from_node to_node =
  (* Adds to_node to value of nodemap for transition t (Transout) with key from_node and vice versa.*)
    let t_initialized_tm = init_transition tm t in
    TreeMap.add (TransIn t) (NodeMap.add_node_to_value (TreeMap.find (TransIn t) t_initialized_tm) to_node from_node) 
                (TreeMap.add (TransOut t) (NodeMap.add_node_to_value (TreeMap.find (TransOut t) t_initialized_tm) from_node to_node) t_initialized_tm)
end

type propkeys = string
(* type of propositions for propmap. 
   has to be out of PropMap scope because of type inference reasons? 
   at least the compiler complains about it. 
*)

module PropMap = struct
(* proposition map. key: string/proposition (propkeys) value: set of nodes where proposition is valid. *)
  type t = (propkeys, NodeSet.t) TreeMap.t

  let empty = TreeMap.empty compare
  let find = TreeMap.find
  let get_keys_as_list pm =
    let l = ref [] in
    TreeMap.iter (fun k v -> l := k :: !l) pm;
    !l
  
  let get_props_of_node pm n = 
  (*inefficient solution. only used for printing purposes. 
    maybe add another map to lts type if needed heavily
  *)
    let ps = ref PropSet.empty in
    TreeMap.iter (fun k v -> if NodeSet.elem n v then ps := PropSet.add k !ps) pm;
    !ps
  
  let add_node_to_prop pm prop node =
    match TreeMap.find_opt prop pm with
    | None -> TreeMap.add prop (NodeSet.add node NodeSet.empty) pm
    | Some(value) -> TreeMap.add prop (NodeSet.add node value) pm
end

type t = {nodes : NodeSet.t; propositions : PropMap.t; transitions : TransitionMap.t}

let create_empty = 
  {nodes = NodeSet.empty; propositions = PropMap.empty; transitions = TransitionMap.empty}

let create_dummy node_count = 
  {nodes = NodeSet.init node_count; propositions = PropMap.empty; transitions = TransitionMap.empty}

let create_dummy_from_nodeset nodeset =
  {nodes = nodeset; propositions = PropMap.empty; transitions = TransitionMap.empty}

let add_node lts node =
  {nodes = NodeSet.add node lts.nodes; propositions = lts.propositions; transitions = lts.transitions}

let add_transition lts transition from_node to_node =
  {nodes = lts.nodes; propositions = lts.propositions; transitions = TransitionMap.set_transition lts.transitions transition from_node to_node}

let add_proposition lts node proposition =
  {nodes = lts.nodes; propositions = PropMap.add_node_to_prop lts.propositions proposition node; transitions = lts.transitions}
let range a b =
(* SEE https://ocaml.org/learn/tutorials/99problems.html*)
  let rec aux a b =
    if a > b then [] else a :: aux (a+1) b  in
  if a > b then List.rev (aux b a) else aux a b

let rand_select list n =
(* SEE https://ocaml.org/learn/tutorials/99problems.html*)
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n-1) t
  in
  let extract_rand list len =
    extract [] (Random.int len) list
  in
  let rec aux n acc list len =
    if n = 0 then acc else
      let picked, rest = extract_rand list len in
      aux (n-1) (picked :: acc) rest (len-1)
  in
  let len = List.length list in
  aux (min n len) [] list len

let rec all_pairs list =
(*returns list of all pairs of elements in list.*)
  match list with
    | [] -> []
    | h :: tl -> (h,h) :: (List.map (fun i -> (h,i)) tl) @ (List.map (fun i -> (i,h)) tl) @ all_pairs tl

let create_random node_count trans_as_string_list props_as_string_list edge_load prop_load =
  let lts = ref (create_dummy node_count) in
  let nodeslist = NodeSet.as_node_list ((!lts).nodes) in
  let prop_load_of_list = int_of_float ((float)(List.length nodeslist) *. prop_load) in (* TODO This can never be the best way to do this. *)
  let all_edges = all_pairs nodeslist in
  let edge_load_of_list = int_of_float ((float)(List.length all_edges) *. edge_load) in
  ignore Random.self_init;
  List.iter (fun p -> List.iter (fun n -> lts := add_proposition !lts n p) (rand_select nodeslist prop_load_of_list)) props_as_string_list;
  List.iter (fun t -> List.iter (fun e -> let (a,b) = e in lts := add_transition !lts t a b) (rand_select all_edges edge_load_of_list)) trans_as_string_list;
  !lts

let get_all_nodes lts =
  lts.nodes
let get_trans_predecessors_of_node lts trans node =
  TransitionMap.get_tpredecessors_of_node lts.transitions trans node

let get_trans_successors_of_node lts trans node =
  TransitionMap.get_tsuccessors_of_node lts.transitions trans node

let get_nodes_of_proposition lts prop =
  PropMap.find prop lts.propositions
  
let to_string lts =
  let string_rep = ref "" in
  let active_tkeys = TransitionMap.get_keys_as_list lts.transitions in
  string_rep := "NODE (TRANSITION [PREDECESSORS] [SUCCESSORS])* PROPS";
  NodeSet.iter (fun n -> 
                  let str = ref (Node.to_string n) in 
                  List.iter (fun t  ->
                              let tpred_str = NodeSet.to_string (TransitionMap.get_tpredecessors_of_node lts.transitions t n) in
                              let tsucc_str = NodeSet.to_string (TransitionMap.get_tsuccessors_of_node lts.transitions t n) in 
                              str := !str ^ " " ^ t ^ "-trans " ^ tpred_str ^ " " ^ tsucc_str) active_tkeys; 
                  str := !str ^ " props " ^ (PropSet.to_string (PropMap.get_props_of_node lts.propositions n)); 
                  string_rep := String.concat "\n" [!string_rep;!str]) lts.nodes;
  !string_rep