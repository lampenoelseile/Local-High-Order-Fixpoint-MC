(** For detailed information see lts.mli *)
open Tcsbasedata
open Tcsset
open Datastructures

(* PROPOSITION MAP *)
module PropMap = struct
  type t = (string, NodeSet.t) TreeMap.t 

  let empty = TreeMap.empty compare
end

type t = {nodes_array: (NodeSet.t * NodeSet.t * PropSet.t) array; propositions : PropMap.t}
let create_dummy node_count = {nodes_array = Array.make (node_count) (NodeSet.empty, NodeSet.empty, PropSet.empty); propositions = PropMap.empty}

let add_edge lts from_node to_node = 
  let get_node l n = l.nodes_array.(n) in
  let set_node l n p s pr = l.nodes_array.(n) <- (p,s,pr) in
  let (from_pred,from_succ,from_prop) = get_node lts from_node in 
  set_node lts from_node from_pred (NodeSet.add to_node from_succ) from_prop;
  let (to_pred,to_succ,to_prop) = get_node lts to_node in
  set_node lts to_node (NodeSet.add from_node to_pred) to_succ to_prop;
  lts

let add_propositions lts node propset_add =
  let get_node l n = l.nodes_array.(n) in
  let set_node l n p s pr = l.nodes_array.(n) <- (p,s,pr) in
  let (pred, succ, propset_old) = get_node lts node in 
  set_node lts node pred succ (PropSet.union propset_old propset_add);
  lts

(* TODO let del_propositions lts node propositions = IF NEEDED*)

let get_predecessors_of_node lts node = let (predecessors,_,_) = lts.nodes_array.(node) in predecessors
let get_successors_of_node lts node = let (_,successors,_) = lts.nodes_array.(node) in successors
let get_propositions_of_node lts node = let (_,_,propositions) = lts.nodes_array.(node) in propositions
let get_nodes_of_propositions lts propositions = NodeSet.empty (* TODO PROPMAP IF NEEDED*)

let get_size lts = Array.length lts.nodes_array

let get_nodeset lts =
  let size = get_size lts in
  let rec add_nodes nodeset = function 
        | 0 -> NodeSet.add  0 nodeset
        | node_count -> add_nodes (NodeSet.add node_count nodeset) (node_count-1)
  in
  add_nodes (NodeSet.empty) (size-1)

let print lts =
    let node_to_string node_id lts_node = (* NOT ABSTRACT OF NODE DEF *)
        let (predecessors,successors,propositions) = lts_node in 
        print_endline ((string_of_int node_id) ^ " " ^
        (NodeSet.to_string predecessors) ^ " " ^
        (NodeSet.to_string successors) ^ " " ^
        (PropSet.to_string propositions)) 
  in 
  let nodes_array = lts.nodes_array in 
  Array.iteri node_to_string nodes_array

(* LTS CREATION *)

(* COPIED FROM https://ocaml.org/learn/tutorials/99problems.html*)
let range a b =
  let rec aux a b =
    if a > b then [] else a :: aux (a+1) b  in
  if a > b then List.rev (aux b a) else aux a b

let rand_select list n =
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
(********************)

let rec all_pairs list =
  match list with
    | [] -> []
    | h :: tl -> (h,h) :: (List.map (fun i -> (h,i)) tl) @ (List.map (fun i -> (i,h)) tl) @ all_pairs tl

let create_random node_count props_as_string_list edge_load prop_load =
  let lts = ref (create_dummy node_count) in
  let nodeslist = NodeSet.as_node_list (get_nodeset !lts) in
  let prop_load_of_list = int_of_float ((float)(List.length nodeslist) *. prop_load) in (* TODO This can never be the best way to do this. *)
  let all_edges = all_pairs nodeslist in
  let edge_load_of_list = int_of_float ((float)(List.length all_edges) *. edge_load) in
  ignore Random.self_init;
  List.iter (fun p -> List.iter (fun n -> lts := add_propositions !lts n (PropSet.of_string_list [p])) (rand_select nodeslist prop_load_of_list)) props_as_string_list;
  List.iter (fun e -> let (a,b) = e in lts := add_edge !lts a b) (rand_select all_edges edge_load_of_list);
  !lts