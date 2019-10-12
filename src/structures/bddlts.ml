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

type node = int

let create number_of_nodes =
  let number_of_basevars = Tools.num_needed_bits number_of_nodes in
  let range_of_basevars_ids = Tools.range 0 (number_of_basevars-1) in
  let range_of_basevarsTo_ids = Tools.range number_of_basevars (2*number_of_basevars-1) in
  let number_of_ord1vars = number_of_nodes in
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
    | (h_v::t_v,h_b::t_b) -> build (MLBDD.dand bdd (if h_b == 1 then h_v else MLBDD.dnot h_v)) t_v t_b
  in

  match (List.rev(list_vars),List.rev(list_bin)) with
    (h_v::t_v, h_b::t_b) -> build (if h_b == 1 then h_v else MLBDD.dnot h_v) t_v t_b 
  | _ -> assert false

let add_proposition lts proposition node = 
  let binary_rep = Tools.int_to_binary_list node in
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

let add_transition lts transition from_node to_node =
  let binary_rep_from_node = Tools.int_to_binary_list from_node in 
  let binary_rep_to_node = Tools.int_to_binary_list to_node in 

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
                          (bdd_fom_bin_rep lts.basevars binary_rep_from_node)
                          (bdd_fom_bin_rep lts.basevarsTo binary_rep_to_node)
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
                        (bdd_fom_bin_rep lts.basevars binary_rep_from_node)
                        (bdd_fom_bin_rep lts.basevarsTo binary_rep_to_node)
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

let complete_sat_list sat_list compl_vars_list = (*To get all nodes from satisfactories, even if bdd is simpliefied / reduced*)
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


let get_tovars lts =
  lts.basevarsTo

let get_sat_states lts bdd =
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
                        List.fold_left 
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
                          ) 
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
  
  (*let to_string lts =
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
  !string_rep**)