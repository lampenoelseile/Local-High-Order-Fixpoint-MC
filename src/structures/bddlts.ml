open Tcsset

type t = 
  {
    manager: MLBDD.man;                         (*BDD Manager (see MLBDD Library)*)
    basevars: MLBDD.t list;                     (*List of boolean vars, representing states (e.g. 011 represents state 3) - lefttoright -> hightolow!*)
    basevarsIDs: int list;
    basevarsTo: MLBDD.t list;                   (*Second of boolean vars, representing states. Needed for transition definition - lefttoright -> hightolow!*)
    basevarsToIDs: int list;
    basevarsToSupport: MLBDD.support;           (*Needed for quantification in modal cases.*)
    ord1vars: MLBDD.t list;                     (*List of boolean vars, state sets (e.g. 1010 means state 1 and 3 are included, 2 and 4 not)*)
    ord1varsIDs: int list;
    propositions: (string,MLBDD.t) TreeMap.t;   (*Map that links a proposition (string) to its BDD representation*)
    transitions: (string,MLBDD.t) TreeMap.t     (*Map that links a transition (string) to its BDD representation*)
  }

type state = int

let create number_of_states =
  let number_of_basevars = Tools.num_needed_bits number_of_states in
  let range_of_basevars_ids = Tools.range 0 (number_of_basevars-1) in
  let range_of_basevarsTo_ids = Tools.range number_of_basevars (2*number_of_basevars-1) in
  let number_of_ord1vars = number_of_states in
  let range_of_ord1vars_ids = Tools.range (2*number_of_basevars) ((2*number_of_basevars)+number_of_ord1vars-1) in
  let man = MLBDD.init() in
  {
    manager = man;
    basevars =  List.fold_right 
                  (fun basevar_id  list_of_basevars -> 
                    (MLBDD.ithvar man basevar_id) :: list_of_basevars
                  )
                  range_of_basevars_ids
                  [];
    basevarsIDs = range_of_basevars_ids;
    basevarsTo = List.fold_right 
                  (fun basevar_id  list_of_basevars -> 
                    (MLBDD.ithvar man basevar_id) :: list_of_basevars
                  )
                  range_of_basevarsTo_ids
                  [];
    basevarsToIDs = range_of_basevarsTo_ids;
    basevarsToSupport = MLBDD.support_of_list range_of_basevarsTo_ids;
    ord1vars =  List.fold_right 
                  (fun ord1var_id list_of_ord1vars -> 
                    (MLBDD.ithvar man ord1var_id) :: list_of_ord1vars
                  )
                  range_of_ord1vars_ids
                  [];
    ord1varsIDs = range_of_ord1vars_ids;
    propositions = TreeMap.empty (String.compare);
    transitions = TreeMap.empty (String.compare);
  }

let bdd_fom_bin_rep list_vars list_bin = (*size of list_bin has to be <= than size of list_vars!*)
  let rec build bdd vars bins =          (*Expects list_vars low_id to high_id and list_bin high_bit to low_bit*)
    match (vars,bins) with
      ([],[]) -> bdd 
    | (h_v::t_v,[]) -> build (MLBDD.dand bdd (MLBDD.dnot h_v)) t_v []
    | ([],h_b::t_b) -> invalid_arg "bdd_from_bin_rep"
    | (h_v::t_v,h_b::t_b) -> build (MLBDD.dand bdd (if h_b = 1 then h_v else MLBDD.dnot h_v)) t_v t_b
  in

  match (List.rev(list_vars),List.rev(list_bin)) with
    (h_v::t_v, h_b::t_b) -> build (if h_b = 1 then h_v else MLBDD.dnot h_v) t_v t_b 
  | _ -> assert false

let add_proposition lts proposition state = 
  let binary_rep = Tools.int_to_binary_list state in
  if TreeMap.mem proposition lts.propositions then
    let prop_bdd = TreeMap.find proposition lts.propositions in
    { 
      manager = lts.manager;
      basevars = lts.basevars;
      basevarsIDs = lts.basevarsIDs;
      basevarsTo = lts.basevarsTo;
      basevarsToIDs = lts.basevarsToIDs;
      basevarsToSupport = lts.basevarsToSupport;
      ord1vars = lts.ord1vars;
      ord1varsIDs = lts.ord1varsIDs;
      propositions =  TreeMap.add 
                        proposition
                        (MLBDD.dor prop_bdd (bdd_fom_bin_rep lts.basevars binary_rep))
                        lts.propositions;
      transitions = lts.transitions
    }
  else
    { 
      manager = lts.manager;
      basevars = lts.basevars;
      basevarsIDs = lts.basevarsIDs;
      basevarsTo = lts.basevarsTo;
      basevarsToIDs = lts.basevarsToIDs;
      basevarsToSupport = lts.basevarsToSupport;
      ord1vars = lts.ord1vars;
      ord1varsIDs = lts.ord1varsIDs;
      propositions =  TreeMap.add 
                        proposition
                        (bdd_fom_bin_rep lts.basevars binary_rep)
                        lts.propositions;
      transitions = lts.transitions
    }

let add_transition lts transition from_state to_state =
  let binary_rep_from_state = Tools.int_to_binary_list from_state in 
  let binary_rep_to_state = Tools.int_to_binary_list to_state in 

  if TreeMap.mem transition lts.transitions then
    let trans_bdd = TreeMap.find transition lts.transitions in
    {
      manager = lts.manager;
      basevars = lts.basevars;
      basevarsIDs = lts.basevarsIDs;
      basevarsTo = lts.basevarsTo;
      basevarsToIDs = lts.basevarsToIDs;
      basevarsToSupport = lts.basevarsToSupport;
      ord1vars = lts.ord1vars;
      ord1varsIDs = lts.ord1varsIDs;
      propositions = lts.propositions;
      transitions = TreeMap.add
                      transition
                      (MLBDD.dor 
                        trans_bdd 
                        (MLBDD.dand 
                          (bdd_fom_bin_rep lts.basevars binary_rep_from_state)
                          (bdd_fom_bin_rep lts.basevarsTo binary_rep_to_state)
                        )
                      )
                      lts.transitions
    }
  else
    {
      manager = lts.manager;
      basevars = lts.basevars;
      basevarsIDs = lts.basevarsIDs;
      basevarsTo = lts.basevarsTo;
      basevarsToIDs = lts.basevarsToIDs;
      basevarsToSupport = lts.basevarsToSupport;
      ord1vars = lts.ord1vars;
      ord1varsIDs = lts.ord1varsIDs;
      propositions = lts.propositions;
      transitions = TreeMap.add
                      transition
                      (MLBDD.dand 
                        (bdd_fom_bin_rep lts.basevars binary_rep_from_state)
                        (bdd_fom_bin_rep lts.basevarsTo binary_rep_to_state)
                      )
                      lts.transitions
    }

let get_man lts =
  lts.manager 

let get_prop lts proposition =
  if TreeMap.mem proposition lts.propositions then 
    TreeMap.find proposition lts.propositions
  else
    invalid_arg "get_prop"

let get_trans lts transition =
  if TreeMap.mem transition lts.transitions then 
    TreeMap.find transition lts.transitions
  else
    invalid_arg "get_prop"

let get_tovars_support lts = 
  lts.basevarsToSupport

let get_tovars_ids_as_array lts =
  Array.of_list lts.basevarsToIDs

let basesat_to_int sat =
  let bit_length = (List.length sat)-1 in
  let value = ref 0 in 
  List.iteri
    (fun id assignment ->
      let (bool_val,_) = assignment in
      let bit = bit_length - id in
      value :=  !value 
                + (if bool_val then int_of_float (2.0**((float)bit)) else 0)
    )
    sat;
  !value

let get_tovars lts =
  lts.basevarsTo

let does_satisfy lts bdd state =
  let state_bdd = bdd_fom_bin_rep lts.basevars (Tools.int_to_binary_list state) in 
  MLBDD.sat (MLBDD.dand bdd state_bdd) <> None

let complete_sat_list sat_list compl_vars_list = (*To get all states from satisfactories, even if bdd is simpliefied / reduced*)
  let rec complete list part_l comp_l =
    match (part_l,comp_l) with 
      (hp::tp,hc::tc) ->  let (boolp,varp) = hp in 
                            if varp = hc then (complete (list @ [hp]) tp tc)
                            else (complete (list @ [(true,hc)]) part_l tc) @ (complete (list @ [(false,hc)]) part_l tc)                   
    | ([], hc::tc) -> (complete (list @ [(true,hc)]) part_l tc) @ (complete (list @ [(false,hc)]) part_l tc)
    | ([],[]) -> [list]
    | _ -> assert false
  in
  complete [] sat_list compl_vars_list

let get_all_sat_states lts bdd = (*TODO: Returns undefined states because of completion *)
  List.fold_right 
    (fun sat list ->
      (basesat_to_int sat) :: list
    )
    (
      List.fold_left
        (fun list sat ->
          list @ (complete_sat_list sat lts.basevarsIDs)
        )
      []
      (MLBDD.allsat bdd)
    )
    []

let to_string lts =
  let header = "LABELED TRANSITION SYSTEM\n" in 
  let prop_str =  "propositions\n" ^
                  TreeMap.fold 
                    (fun key value str ->
                      str ^ key ^ ": {" ^
                        Tools.strip_last_char
                        (List.fold_left 
                          (fun str i -> 
                            str ^ (string_of_int i) ^ ","
                          ) 
                          "" 
                          (List.fold_right 
                            (fun sat list ->
                              (basesat_to_int sat) :: list
                            )
                            (
                              List.fold_left
                                (fun list sat ->
                                  list @ (complete_sat_list sat lts.basevarsIDs)
                                )
                              []
                              (MLBDD.allsat value)
                            )
                            []
                          )) 
                      ^ "}\n"
                    ) 
                    lts.propositions
                    ""
  in
  let trans_str = "transitions\n" ^
                  TreeMap.fold
                    (fun key value str ->
                      str ^ List.fold_left
                              (fun str sat ->
                                let (from_sat,to_sat) = Tools.split_list_in_half sat in
                                str ^ (string_of_int (basesat_to_int from_sat)) ^ " --" ^ key ^"--> " 
                                ^ string_of_int (basesat_to_int to_sat) ^ "\n"
                              )
                              ""
                              (
                              List.fold_left
                                (fun list sat ->
                                  list @ (complete_sat_list sat (lts.basevarsIDs @ lts.basevarsToIDs))
                                )
                              []
                              (MLBDD.allsat value)
                            )
                    )
                    lts.transitions
                    ""
  in
  header ^ prop_str ^ trans_str
  
 (*module HO = struct
  
  type base = MLBDD.t

  type refer = 
    RefBase of MLBDD.t
  | RefFun of (ref_t -> ref_t)

  type map_t = 
    MapBase of MLBDD.t
  | MapFun of (map_t,map_t) TreeMap.t

  type t = 
    Ref of ref_t 
  | Map of map_t

  let detype_ref = function 
    Ref ref -> ref
  | _ -> assert false

  let detype_map = function 
    Map map -> map
  | _ -> assert false 

  let get_value hot args =
    match hot with 
      Ref ref ->  let rec apply_arg_list hot arglist = (*This gives me goosebumps*)
                    match (hot,arglist) with 
                      (RefBase bdd, []) -> hot
                    | (RefFun f, h::t) -> apply_arg_list (f (detype_ref h)) t
                    | (RefFun f, []) -> hot
                    | _ -> assert false 
                  in
                  Ref (apply_arg_list ref args)
    | Map map ->  let rec get_value_for_args = function
                    | MapBase(b) -> (function
                                  | [] -> MapBase(b)
                                  | _ -> assert false)
                    | MapFun(map) -> (function
                                  | [] -> assert false
                                  | h :: [] -> TreeMap.find (detype_map h) map
                                  | h :: t -> get_value_for_args (TreeMap.find (detype_map h) map) t)
                  in 
                  Map (get_value_for_args map args)

  let rec compare_map = function
      MapBase ns_one ->  (function
                  MapBase ns_two -> Int.compare (MLBDD.id ns_one) (MLBDD.id ns_two)
                  | MapFun map -> assert false)

    | MapFun map_one -> (function
                      MapBase ns_two -> assert false
                      | MapFun map_two -> let keys_one = List.sort (fun a b -> compare a b) (Tcsbasedata.Iterators.to_list 
                              (TreeMap.to_key_iterator map_one)) in
                              let keys_two = List.sort (fun a b -> compare a b) (Tcsbasedata.Iterators.to_list 
                              (TreeMap.to_key_iterator map_two)) in
                              let rec helper = function
                                h1 :: t1 -> (function 
                                            h2 :: t2 -> let value = compare (TreeMap.find h1 map_one) (TreeMap.find h2 map_two) 
                                                        in if value != 0 then value else helper t1 t2
                                          |  [] -> 1
                                          )
                              | [] -> (function 
                                        h :: t -> -1
                                      | [] -> 0
                                      ) 
                              in 
                              helper keys_one keys_two            
                        )
  
  let of_bdd bdd = 
    Ref (RefBase bdd)  (*uff...*)

  let to_bdd ho =
    match ho with 
      Ref r -> (match r with RefBase r' -> r' | _ -> assert false)
    | Map m -> (match m with MapBase m' -> m' | _ -> assert false)  
   
  let empty_ns lts = 
    List.fold_left 
      (fun bdd var ->
        MLBDD.dand bdd (MLBDD.dnot var)
      )
      (MLBDD.dnot (List.hd lts.ord1vars))
      (List.tl lts.ord1vars)
  
  let empty_fun = MapFun(TreeMap.empty compare_map)

  let rec is_defined_for_args = function
    | MapBase(ns) -> (function
                  | [] -> true
                  | _ ->  assert false)
    | MapFun(map) -> (function
                  | [] -> assert false
                  | h :: [] -> TreeMap.mem h map 
                  | h :: t ->  let value = TreeMap.mem h map in
                                value && is_defined_for_args (TreeMap.find h map) t)

  let rec get_value_for_args = function
    | MapBase(ns) -> (function
                  | [] -> MapBase(ns)
                  | _ -> assert false)
    | MapFun(map) -> (function
                  | [] -> assert false
                  | h :: [] -> TreeMap.find h map
                  | h :: t -> get_value_for_args (TreeMap.find h map) t)

  let rec set_value_for_args value = function
    | MapBase(ns) -> (function
                    | [] -> value
                    | _ ->  assert false)
    | MapFun(map) ->  (function
                    | [] ->  assert false
                    | h :: [] -> MapFun(TreeMap.add h value map)
                    | h :: t -> if is_defined_for_args (MapFun map) [h] then
                                  MapFun(TreeMap.add h (set_value_for_args value (TreeMap.find h map) t) map)
                                else
                                  let new_map = empty_fun in 
                                  MapFun(TreeMap.add h (set_value_for_args value new_map t) map))
  
  let rec get_defined_arguments = function
    MapBase(ns) -> [[]]
  | MapFun(map) -> if TreeMap.is_empty map then []
                else begin
                  let args = Tcsbasedata.Iterators.to_list 
                              (TreeMap.to_key_iterator map) 
                  in
                  List.fold_left 
                    (fun list arg ->
                      (List.map 
                        (fun arg_list -> arg :: arg_list) 
                        (get_defined_arguments (get_value_for_args (MapFun map) [arg]))
                      ) 
                      @ list
                    ) 
                    [] 
                    args
                end
end*)