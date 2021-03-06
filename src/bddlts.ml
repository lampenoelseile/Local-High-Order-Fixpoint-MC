open Tcsset

type t = 
  {
    manager: MLBDD.man;                         (*BDD Manager (see MLBDD Library)*)
    basevars: MLBDD.t list;                     (*List of boolean vars, representing states (e.g. 011 represents state 3) - lefttoright -> hightolow!*)
    basevarsIDs: int list;                      (*Ids of basevars.*)
    basevarsTo: MLBDD.t list;                   (*Second of boolean vars, representing states. Needed for transition definition - lefttoright -> hightolow!*)
    basevarsToIDs: int list;                    (*Ids of basevarsTo.*)
    basevarsToSupport: MLBDD.support;           (*Needed for quantification in modal cases.*)
    propositions: (string,MLBDD.t) TreeMap.t;   (*Map that links a proposition (string) to its BDD representation*)
    transitions: (string,MLBDD.t) TreeMap.t;    (*Map that links a transition (string) to its BDD representation*)
    statescoded: MLBDD.t list;                  (*Each state as binary encoding of the basevars*)
    allstates: MLBDD.t                          (*BDD representing all states to avoid undefined ones because of too much bits (big AND)*)
  }

type state = int

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

let create number_of_states =
  let number_of_basevars = Tools.num_needed_bits number_of_states in
  let range_of_basevars_ids = Tools.range 0 (number_of_basevars-1) in
  let range_of_basevarsTo_ids = Tools.range number_of_basevars (2*number_of_basevars-1) in
  let man = MLBDD.init() in
  let basevars = List.fold_right 
                  (fun basevar_id  list_of_basevars -> 
                    (MLBDD.ithvar man basevar_id) :: list_of_basevars
                  )
                  range_of_basevars_ids
                  [];
  in
  let basevarsTo = List.fold_right 
                  (fun basevar_id  list_of_basevars -> 
                    (MLBDD.ithvar man basevar_id) :: list_of_basevars
                  )
                  range_of_basevarsTo_ids
                  [];
  in
  let statescoded = List.fold_left
                    (fun code_list state -> 
                      bdd_fom_bin_rep basevars (Tools.int_to_binary_list state) :: code_list
                    )
                    []
                    (Tools.range 0 (number_of_states-1))
  in
  {
    manager = man;
    basevars =  basevars;
    basevarsIDs = range_of_basevars_ids;
    basevarsTo = basevarsTo;
    basevarsToIDs = range_of_basevarsTo_ids;
    basevarsToSupport = MLBDD.support_of_list range_of_basevarsTo_ids;
    propositions = TreeMap.empty (String.compare);
    transitions = TreeMap.empty (String.compare);
    statescoded = statescoded;
    allstates = List.fold_left
    (fun bdd elem ->
      MLBDD.dor bdd elem
    )
    (List.hd statescoded)
    (List.tl statescoded)
  }

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
      propositions =  (TreeMap.add 
                        proposition
                        (MLBDD.dor prop_bdd (bdd_fom_bin_rep lts.basevars binary_rep))
                        lts.propositions);
      transitions = lts.transitions;
      statescoded = lts.statescoded;
      allstates = lts.allstates
    }
  else
    { 
      manager = lts.manager;
      basevars = lts.basevars;
      basevarsIDs = lts.basevarsIDs;
      basevarsTo = lts.basevarsTo;
      basevarsToIDs = lts.basevarsToIDs;
      basevarsToSupport = lts.basevarsToSupport;
      propositions =  (TreeMap.add 
                        proposition
                        (bdd_fom_bin_rep lts.basevars binary_rep)
                        lts.propositions);
      transitions = lts.transitions;
      statescoded = lts.statescoded;
      allstates = lts.allstates
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
      propositions = lts.propositions;
      transitions = TreeMap.add
                      transition
                      (
                        MLBDD.dor 
                          trans_bdd 
                          (
                            MLBDD.dand 
                              (bdd_fom_bin_rep lts.basevars binary_rep_from_state)
                              (bdd_fom_bin_rep lts.basevarsTo binary_rep_to_state)
                          )
                      )
                      lts.transitions;
      statescoded = lts.statescoded;
      allstates = lts.allstates
    }
  else
    {
      manager = lts.manager;
      basevars = lts.basevars;
      basevarsIDs = lts.basevarsIDs;
      basevarsTo = lts.basevarsTo;
      basevarsToIDs = lts.basevarsToIDs;
      basevarsToSupport = lts.basevarsToSupport;
      propositions = lts.propositions;
      transitions = TreeMap.add
                      transition
                      (
                        MLBDD.dand 
                          (bdd_fom_bin_rep lts.basevars binary_rep_from_state)
                          (bdd_fom_bin_rep lts.basevarsTo binary_rep_to_state)
                      )
                      lts.transitions;
      statescoded = lts.statescoded;
      allstates = lts.allstates
    }

let get_man lts =
  lts.manager 

let get_prop lts proposition =
  if TreeMap.mem proposition lts.propositions then 
    MLBDD.dand (TreeMap.find proposition lts.propositions) lts.allstates
  else
    MLBDD.dfalse lts.manager (*TODO: Is this a problem regarding undefined states?*)

let get_trans lts transition =
  if TreeMap.mem transition lts.transitions then 
    TreeMap.find transition lts.transitions
  else
    MLBDD.dfalse lts.manager (*TODO: Is this a problem regarding undefined states?*)

let get_statescoded lts =
  lts.statescoded
  
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

let get_allstates lts =
  lts.allstates

let get_diamond_trans_of lts trans bdd = 
  (MLBDD.exists 
    (lts.basevarsToSupport)
    ( 
      MLBDD.dand 
        (
          MLBDD.dand 
            (get_trans lts trans) 
            (MLBDD.permute (Array.of_list lts.basevarsToIDs) bdd)
        ) 
        (lts.allstates)
    )
  )

let get_box_trans_of lts trans bdd =
  MLBDD.forall 
    (lts.basevarsToSupport)
    (
      MLBDD.dand
      (
        MLBDD.dor
          (MLBDD.dnot (get_trans lts trans)) 
          (MLBDD.permute (Array.of_list lts.basevarsToIDs) bdd)
      )
      (lts.allstates)
    )
  
let complete_sat_list sat_list compl_vars_list = 
  (*To get all satisfying states, even if bdd is simpliefied / reduced*)
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

let get_all_sats lts bdd = (*TODO: Returns undefined states because of completion *)
  List.fold_right 
    (fun sat list -> (basesat_to_int sat) :: list)
    (
      List.fold_left
        (fun list sat ->
          list @ (List.filter 
                    (fun elem -> (basesat_to_int elem) < (List.length lts.statescoded)) 
                    (complete_sat_list sat lts.basevarsIDs))
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
                          (
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
                          ) 
                      ^ "}\n"
                    ) 
                    lts.propositions
                    ""
  in
  let trans_str = "transitions\n" ^
                  TreeMap.fold
                    (fun key value str ->
                      str 
                      ^ List.fold_left
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