(* For high-level info about public functions see lts.mli *)
open Tcsbasedata
open Tcsset
open Basedata
open Tools
open Hfl

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

  let fold = TreeMap.fold

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
  
  let turn_transitions tm =
    TreeMap.mapi (fun key value -> 
                    match key with
                      TransIn t -> TreeMap.find (TransOut t) tm
                    | TransOut t -> TreeMap.find (TransIn t) tm
                  ) tm
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
  let fold = TreeMap.fold

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

let add_transition lts transition from_node to_node = (*TODO Check if exist *)
  {nodes = lts.nodes; propositions = lts.propositions; transitions = TransitionMap.set_transition lts.transitions transition from_node to_node}

let add_proposition lts node proposition =
  {nodes = lts.nodes; propositions = PropMap.add_node_to_prop lts.propositions proposition node; transitions = lts.transitions}

let make_simple lts =
  let module NMap = Map.Make(Node) in
  let counter = ref 0 in
  let node_map = NodeSet.fold 
              (fun map node ->
                counter := !counter + 1;
                NMap.add node (Node.SimpleNode (!counter-1)) map
              ) 
              NMap.empty lts.nodes
  in
  let new_nodes = NodeSet.map 
                  (fun node -> NMap.find node node_map) 
                  lts.nodes
  in 
  let new_propmap = List.fold_left 
                    (fun propmap prop -> 
                      NodeSet.fold 
                      (fun propmap2 node -> 
                        PropMap.add_node_to_prop propmap2 prop (NMap.find node node_map)
                      ) 
                      propmap (PropMap.find prop lts.propositions)
                    ) 
                    PropMap.empty (PropMap.get_keys_as_list lts.propositions)
  in
  let new_trans_map = List.fold_left
                      (fun tmap trans -> 
                        NodeSet.fold 
                        (fun tmap2 node ->
                          NodeSet.fold 
                          (fun tmap3 node2 -> TransitionMap.set_transition tmap2 trans (NMap.find node node_map) (NMap.find node2 node_map)) 
                          tmap2 
                          (TransitionMap.get_tsuccessors_of_node lts.transitions trans node)
                        ) 
                        tmap lts.nodes
                      )
                      TransitionMap.empty (TransitionMap.get_keys_as_list lts.transitions)
  in
    print_endline "\n";
  print_endline 
  (List.fold_left 
    (fun str tuple -> 
      let (keyy,value) = tuple in 
      str ^ Node.to_string keyy ^ " -> " ^ Node.to_string value ^ "; ") "" (NMap.bindings node_map)
  );
  {nodes = new_nodes; propositions = new_propmap; transitions = new_trans_map}

let turn_transitions lts =
  {nodes = lts.nodes; propositions = lts.propositions; transitions = (TransitionMap.turn_transitions lts.transitions)}

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

let to_bddlts lts =
  let lts = make_simple lts in 
  let bddlts = Bddlts.create (Basedata.NodeSet.size(lts.nodes)) in 
  let bddlts = PropMap.fold
                  (fun prop nodeset bddlts1 -> 
                    NodeSet.fold 
                      (fun bddlts2 node ->
                        Bddlts.add_proposition bddlts2 prop 
                          (match node with
                            Node.SimpleNode i -> i
                          | _ -> assert false
                          )
                      )
                      bddlts1
                      nodeset
                  )
                  lts.propositions
                  bddlts
  in
  let bddlts =  NodeSet.fold 
                  (fun bddlts node ->
                    match node with 
                      Node.SimpleNode n ->  TransitionMap.fold
                                              (fun trans nm bddlts ->
                                                match trans with 
                                                  TransIn t -> NodeSet.fold
                                                                (fun bddlts node2 ->
                                                                  Bddlts.add_transition bddlts t
                                                                  (match node2 with Node.SimpleNode n' -> n' | _ -> assert false) n
                                                                )
                                                                bddlts
                                                                (TransitionMap.get_tpredecessors_of_node lts.transitions t node)
                                                | _ -> bddlts
                                              )
                                              lts.transitions
                                              bddlts
                    | _ -> assert false
                  )
                  bddlts
                  (lts.nodes)
  in bddlts