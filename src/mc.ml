open Basedata
open Hfl
  module F = Formula
  module S = Semantics
  module SemS = SemanticsSet
module V = Verbose
open Tcsset


module VarMap  = struct (*TODO: Add typecheck between var_t and value and exceptions*)
  (*Simple map, which pairs variable (formula) with its current (semantic) value. 
    Based on TreeMap. (SEE TCSLib)
  *)
  type t = (F.t, S.t) TreeMap.t
    (* varmap type. (key:variable, value: semantic object) *)

  let empty = TreeMap.empty compare
    (* creates empty treemap *)

  let mem = TreeMap.mem
    (*check if variable is defined in map *)

  let set ?(v_lvl=V.None) argument value map = 
    (* sets / initializes value for given argument*)
    match argument with
      F.Var(var_name,var_t,fp_t) ->  TreeMap.add argument value map  
    | _ ->  assert false 

  let get ?(v_lvl=V.None) argument map =
    (* Returns current value for given argument. safetychecks included*)
    match argument with
      F.Var(var_name,var_t,fp_t) -> if mem argument map then
                                      TreeMap.find argument map
                                    else 
                                      assert false 
    | _ ->  assert false
end

let rec model_check formula lts args env v_lvl indent =
  (* Lazy model checker for HFL. Checks formula against lts
     with given arguments args. 
     See public signature below.
  *)
  match formula with
    F.Const(b) -> if b then begin 
                    let value = Lts.get_all_nodes lts in
                    V.console_out V.Detailed v_lvl 
                      (fun () -> indent ^ "Case: Top - Value: " ^ NodeSet.to_string value);
                    (value,env)
                  end 
                  else begin
                    V.console_out V.Detailed v_lvl 
                      (fun () -> indent ^ "Case: Bot - Value: {}");
                    (NodeSet.empty,env)
                  end

  | F.Prop(prop) -> let value = Lts.get_nodes_of_proposition lts prop in 
                    V.console_out V.Detailed v_lvl 
                      (fun () -> indent ^ "Case: Prop " ^ prop ^ " - Value: " ^ NodeSet.to_string value);
                    (value,env)
                    
  | F.Var(var,var_t,fp_t) -> V.console_out V.Detailed v_lvl 
                          (fun () -> indent ^ "Case: Var " ^ var);
                        let start_time = Unix.gettimeofday() in
                        let current_val = (VarMap.get (F.Var(var, var_t,fp_t)) env) in
                        if S.is_defined_for_args current_val args then begin 
                          match S.get_value_for_args current_val args with
                            S.Base(ns) -> 
                                          let end_time = Unix.gettimeofday () in
                                          V.console_out V.Detailed v_lvl 
                                            (fun () -> indent ^ "Case Var - Value: " ^ NodeSet.to_string ns);
                                          V.duration_out V.Detailed v_lvl start_time end_time indent;
                                          (ns,env)
                          | S.Fun(map) -> assert false
                        end else begin
                          match fp_t with 
                            F.NoFP -> assert false 
                          | F.LFP ->  let end_time = Unix.gettimeofday () in
                                      V.console_out V.Detailed v_lvl 
                                        (fun () -> indent ^ "Case Var - Value: {} (undef)");
                                      V.duration_out V.Detailed v_lvl start_time end_time indent;
                                      (
                                        NodeSet.empty, 
                                        VarMap.set ~v_lvl (Var(var,var_t,fp_t)) 
                                          (S.set_value_for_args (Base NodeSet.empty) current_val args) env
                                      )
                          | F.GFP ->  let end_time = Unix.gettimeofday () in
                                      V.console_out V.Detailed v_lvl 
                                        (fun () -> indent ^ "Case Var - Value: S (undef)");
                                      V.duration_out V.Detailed v_lvl start_time end_time indent;
                                      (
                                        (Lts.get_all_nodes lts), 
                                        VarMap.set ~v_lvl (Var(var,var_t,fp_t)) 
                                          (S.set_value_for_args (Base (Lts.get_all_nodes lts)) current_val args) env
                                      )
                        end

  | F.Neg(phi) -> V.console_out V.Detailed v_lvl 
                    (fun () -> indent ^ "Case: Negation of " ^ F.to_string phi);
                  let (val1, env) = (model_check phi lts args env v_lvl(indent ^ "  ")) in
                  let value = NodeSet.diff (Lts.get_all_nodes lts) val1 in
                  V.console_out V.Detailed v_lvl 
                    (fun () -> indent ^ "Case Negation - Value: " ^ NodeSet.to_string value);
                  (value,env)

  | F.Conj(phi_1, phi_2) -> V.console_out V.Detailed v_lvl 
                              (fun () ->  indent ^ "Case: Conj of " ^ F.to_string phi_1 
                                          ^ " and " ^ F.to_string phi_2);
                            let (val1, env) = model_check phi_1 lts args env v_lvl (indent ^ "  ") in 
                            let (val2, env) = model_check phi_2 lts args env v_lvl (indent ^ "  ") in
                            let value = NodeSet.inter val1 val2 in
                            V.console_out V.Detailed v_lvl 
                              (fun () ->  indent ^ "Case Conj - Value: " ^ NodeSet.to_string value);
                            (value,env)

  | F.Disj(phi_1, phi_2) -> V.console_out V.Detailed v_lvl 
                              (fun () ->  indent ^ "Case: Disj of " ^ F.to_string phi_1 ^ " and " ^ F.to_string phi_2);
                            let (val1, env) = model_check phi_1 lts args env v_lvl (indent ^ "  ") in 
                            let (val2, env) = model_check phi_2 lts args env v_lvl (indent ^ "  ") in
                            let value = NodeSet.union val1 val2 in
                            V.console_out V.Detailed v_lvl 
                              (fun () ->  indent ^ "Case Disj - Value: " ^ NodeSet.to_string value);
                            (value,env)

  | F.Impl(phi_1, phi_2) -> V.console_out V.Detailed v_lvl 
                              (fun () ->  indent ^ "Case: Impl of " ^ F.to_string phi_1 ^ " and " ^ F.to_string phi_2);
                            let (val1, env) = model_check (F.Neg(phi_1)) lts args env v_lvl (indent ^ "  ") in 
                            let (val2, env) = model_check phi_2 lts args env v_lvl (indent ^ "  ") in
                            let value = NodeSet.union val1 val2 in 
                            V.console_out V.Detailed v_lvl 
                              (fun () ->  indent ^ "Case Impl - Value: " ^ NodeSet.to_string value);
                            (value,env)

  | F.Equiv(phi_1, phi_2) ->  V.console_out V.Detailed v_lvl 
                                (fun () ->  indent ^ "Case: Equiv of " ^ F.to_string phi_1 ^ " and " ^ F.to_string phi_2);
                              let (val1, env) = model_check (F.Impl(phi_1,phi_2)) lts args env v_lvl (indent ^ "  ") in 
                              let (val2, env) = model_check (F.Impl(phi_2,phi_1)) lts args env v_lvl (indent ^ "  ") in
                              let value = NodeSet.inter  val1 val2 in 
                              V.console_out V.Detailed v_lvl 
                                (fun () ->  indent ^ "Case Equiv - Value: " ^ NodeSet.to_string value);
                              (value, env)

  | F.Diamond(trans, phi) ->  V.console_out V.Detailed v_lvl 
                                (fun () ->  indent ^ "Case: Diamond " ^ trans ^ " of " ^ F.to_string phi);
                              let (val_phi, env) = model_check phi lts args env v_lvl (indent ^ "  ") in
                              let val_diamond_phi = 
                                NodeSet.fold 
                                  (fun set node -> NodeSet.union set (Lts.get_trans_predecessors_of_node lts trans node)) 
                                  NodeSet.empty val_phi 
                              in
                              V.console_out V.Detailed v_lvl 
                                (fun () ->  indent ^ "Case Diamond - Value: " ^ NodeSet.to_string val_diamond_phi);
                              (val_diamond_phi, env)

  | F.Box(trans, phi) ->  V.console_out V.Detailed v_lvl 
                            (fun () -> indent ^ "Case: Box " ^ trans ^ " of " ^ F.to_string phi);
                          let (value,env) = model_check (F.Neg(F.Diamond(trans, F.Neg(phi)))) 
                            lts args env v_lvl  (indent ^ "  ") 
                          in 
                          V.console_out V.Detailed v_lvl 
                            (fun () -> indent ^ "Case Box - Value: " ^ NodeSet.to_string value);
                          (value,env)

  | F.Mu(var,var_t,phi) ->  V.console_out V.Detailed v_lvl 
                                (fun () -> indent ^ "Case: LFP of " ^ F.to_string phi 
                                    ^ " regarding variable " ^ var);
                            let x = F.Var(var,var_t,LFP) in
                            let rec repeat_until map =
                              (*repeat until approximation is found (i.e. fixpoint) 
                                returns env including fp approximation for x.
                              *)
                              let start_time = Unix.gettimeofday () in
                              let update = ref 
                                (match var_t with 
                                  F.Base -> S.empty_base | _ -> S.empty_fun) 
                              in
                              let defined_args = (S.get_defined_arguments (VarMap.get ~v_lvl x map)) in
                              List.iter
                                (*for each argument list*) 
                                (fun arg_list ->
                                    V.console_out V.Detailed v_lvl 
                                      (fun () -> indent ^ "  " ^ "LFP Arg: [" 
                                        ^ List.fold_left (fun str arg -> str ^ S.to_string arg ^ ";") 
                                    "" arg_list ^ "]");
                                  let (value, env_tmp) = 
                                    model_check phi lts arg_list map v_lvl (indent ^ "   " ^ "  ")
                                    (*value and (updated env) of approx for current argument*) 
                                  in
                                  let defined_args_tmp = S.get_defined_arguments (VarMap.get ~v_lvl x env_tmp) in
                                    List.iter (*add new arguments to update, which occured first time in this iteration*) 
                                    (fun arg_list_tmp -> 
                                      update := 
                                      S.set_value_for_args 
                                        (S.get_value_for_args (VarMap.get ~v_lvl x env_tmp) arg_list_tmp) 
                                        !update arg_list_tmp
                                    ) 
                                  (List.filter (fun x -> not (List.mem x defined_args)) defined_args_tmp);
                                  update := S.set_value_for_args (S.Base value) !update arg_list
                                ) 
                                defined_args;
                              if S.compare !update (VarMap.get ~v_lvl x map) == 0 then
                              begin (*last and second to last iteration are equal *)
                                let end_time = Unix.gettimeofday () in 
                                V.console_out V.Detailed v_lvl (fun () -> indent ^ "FP found.");
                                V.duration_out V.Detailed v_lvl start_time end_time (indent ^ "FP Approx Step - ");
                                map
                              end else
                              begin (*last and second to last iteration differ *)
                                let end_time = Unix.gettimeofday () in
                                V.console_out V.Detailed v_lvl (fun () -> indent ^ "Approximation changed.");
                                V.duration_out V.Detailed v_lvl start_time end_time (indent ^ "FP Approx Step - ");
                                repeat_until (VarMap.set ~v_lvl x !update map)
                              end
                            in
                            let initialized_map = VarMap.set ~v_lvl x 
                                  (S.set_value_for_args 
                                    S.empty_base 
                                    (match var_t with F.Base -> S.empty_base | _ -> S.empty_fun) args) 
                                    env 
                            in
                            let approx = VarMap.get ~v_lvl x (repeat_until initialized_map) in
                            (match approx with 
                              S.Base ns -> V.console_out V.Detailed v_lvl 
                                            (fun () -> "FP - Value:" ^ NodeSet.to_string ns)
                            | _ -> V.sem_log_out V.Detailed v_lvl approx (F.Mu(var,var_t,phi)));

                            (match S.get_value_for_args approx args with
                              S.Base ns -> (ns,env)
                            | S.Fun(map)-> assert false)
                            
  | F.Nu(var,var_t,phi) ->  V.console_out V.Detailed v_lvl 
                                (fun () -> indent ^ "Case: GFP of " ^ F.to_string phi 
                                    ^ " regarding variable " ^ var);
                            let x = F.Var(var,var_t,GFP) in
                            let rec repeat_until map =
                              (*repeat until approximation is found (i.e. fixpoint) 
                                returns env including fp approximation for x.
                              *)
                              let start_time = Unix.gettimeofday () in
                              let update = ref 
                                (match var_t with 
                                  F.Base -> S.Base (Lts.get_all_nodes lts) | _ -> S.empty_fun) 
                              in
                              let defined_args = (S.get_defined_arguments (VarMap.get ~v_lvl x map)) in
                              List.iter
                                (*for each argument list*) 
                                (fun arg_list ->
                                    V.console_out V.Detailed v_lvl 
                                      (fun () -> indent ^ "  " ^ "LFP Arg: [" 
                                        ^ List.fold_left (fun str arg -> str ^ S.to_string arg ^ ";") 
                                    "" arg_list ^ "]");
                                  let (value, env_tmp) = 
                                    model_check phi lts arg_list map v_lvl (indent ^ "   " ^ "  ")
                                    (*value and (updated env) of approx for current argument*) 
                                  in
                                  let defined_args_tmp = S.get_defined_arguments (VarMap.get ~v_lvl x env_tmp) in
                                    List.iter (*add new arguments to update, which occured first time in this iteration*) 
                                    (fun arg_list_tmp -> 
                                      update := 
                                      S.set_value_for_args 
                                        (S.get_value_for_args (VarMap.get ~v_lvl x env_tmp) arg_list_tmp) 
                                        !update arg_list_tmp
                                    ) 
                                  (List.filter (fun x -> not (List.mem x defined_args)) defined_args_tmp);
                                  update := S.set_value_for_args (S.Base value) !update arg_list
                                ) 
                                defined_args;
                              if S.compare !update (VarMap.get ~v_lvl x map) == 0 then
                              begin (*last and second to last iteration are equal *)
                                let end_time = Unix.gettimeofday () in 
                                V.console_out V.Detailed v_lvl (fun () -> indent ^ "FP found.");
                                V.duration_out V.Detailed v_lvl start_time end_time (indent ^ "FP Approx Step - ");
                                map
                              end else
                              begin (*last and second to last iteration differ *)
                                let end_time = Unix.gettimeofday () in
                                V.console_out V.Detailed v_lvl (fun () -> indent ^ "Approximation changed.");
                                V.duration_out V.Detailed v_lvl start_time end_time (indent ^ "FP Approx Step - ");
                                repeat_until (VarMap.set ~v_lvl x !update map)
                              end
                            in
                            let initialized_map = VarMap.set ~v_lvl x 
                                  (S.set_value_for_args 
                                    (S.Base (Lts.get_all_nodes lts))
                                    (match var_t with F.Base -> S.Base (Lts.get_all_nodes lts) | _ -> S.empty_fun) args) 
                                    env 
                            in
                            let approx = VarMap.get ~v_lvl x (repeat_until initialized_map) in
                            (match approx with 
                              S.Base ns -> V.console_out V.Detailed v_lvl 
                                            (fun () -> "FP - Value:" ^ NodeSet.to_string ns)
                            | _ -> V.sem_log_out V.Detailed v_lvl approx (F.Mu(var,var_t,phi)));

                            (match S.get_value_for_args approx args with
                              S.Base ns -> (ns,env)
                            | S.Fun(map)-> assert false)

  | F.Lambda(var,var_t,phi) ->  V.console_out V.Detailed v_lvl 
                                  (fun () ->  indent ^ "Case: Lambda " ^ var ^ " of " 
                                              ^ F.to_string phi );
                                let (value,env) = model_check phi lts (List.tl args) (VarMap.set (F.Var(var,var_t,F.NoFP)) 
                                              (List.nth args 0) env) v_lvl (indent ^ "  ")
                                in 
                                V.console_out V.Detailed v_lvl 
                                  (fun () ->  indent ^ "Case Lambda " ^ var ^ " - Value: " ^ NodeSet.to_string value);
                                (value,env)

  | F.App(phi_1, phi_2) ->  V.console_out V.Detailed v_lvl 
                              (fun () ->  indent ^ "Case: App of " ^ F.to_string phi_2 ^ " to " 
                                          ^ F.to_string phi_1);
                            let phi_2_sem = fully_calc_sem phi_2 lts [] env v_lvl (indent ^ "  ") in 
                            let (value, env) = model_check phi_1 lts (phi_2_sem :: args) env v_lvl (indent ^ "  ") in
                            V.console_out V.Detailed v_lvl 
                              (fun () ->  indent ^ "Case App - Value: " 
                                          ^ NodeSet.to_string value);
                            (value,env)

and fully_calc_sem formula lts args env v_lvl indent =
  let rec helper formula lts args env v_lvl = 
    match formula with
      F.Lambda(var, var_t, phi)  ->   let all_args = SemS.all_of_type lts var_t in
                                      SemS.fold 
                                        (fun arg sem -> S.set_value_for_args
                                                          (helper phi lts (args) 
                                                            (VarMap.set ~v_lvl (F.Var(var,var_t,NoFP)) arg env) v_lvl
                                                          ) 
                                                          sem [arg]
                                        ) 
                                        all_args (S.empty_fun) 
                                    
    | F.Mu(var, var_t, phi) ->  let (ns,_) = model_check 
                                              (F.Mu(var, var_t, phi)) 
                                              lts (args) 
                                              env V.None ("  " ^ indent)
                                in S.Base ns
                                (*TODO: only fp arguments of type * -> base are possible with this *)
                                (*TODO: GFP?!*)
                                  
    | F.Var(var,var_t,fp_t) ->  if VarMap.mem (F.Var(var,var_t,fp_t)) env then 
                                  S.get_value_for_args (VarMap.get (F.Var(var,var_t,fp_t)) env) args
                                else assert false

    | F.App(phi_1,phi_2) -> let (value,_) = 
                              model_check phi_1 lts ((helper phi_2 lts [] env v_lvl) :: args) env V.None (indent ^ "  ")
                            in S.Base value

    | _ -> let (value,_) = model_check formula lts [] env V.None (indent ^ "  ") in S.Base(value)
  in 
  V.console_out V.Detailed v_lvl (fun () -> indent ^ "Calculate: " ^ F.to_string formula ^ " completely...");
  let start_time = Unix.gettimeofday() in
  let value = helper formula lts args env v_lvl in
  let end_time = Unix.gettimeofday() in
  (match value with 
    S.Base(ns) -> V.console_out V.Detailed v_lvl (fun () -> indent ^ "Value: " ^ NodeSet.to_string ns)
  | S.Fun(map) -> V.console_out V.Detailed v_lvl (fun () -> indent ^ "Value: see ./outputs/" ^ F.to_string formula ^ ".log");
                  V.sem_log_out V.Detailed v_lvl value formula);
  V.duration_out V.Detailed v_lvl start_time end_time indent;
  value

let fully_calc_sem ?(v_lvl=V.Debug) formula lts =
  fully_calc_sem formula lts [] VarMap.empty v_lvl "  "

let model_check ?(v_lvl=V.Info) formula lts =
  V.console_out V.Info v_lvl (fun () -> "MODEL CHECKER INPUT:");
  V.console_out V.Info v_lvl (fun () -> "LTS: " ^ Lts.to_string lts ^"\n");
  V.console_out V.Info v_lvl (fun () -> "FORMULA: " ^ F.to_string formula ^ "\n\n");
  V.console_out V.Info v_lvl (fun () -> "Start model checker...");
  let start_time = Unix.gettimeofday () in
  let (value,_) = model_check formula lts [] VarMap.empty v_lvl "  " in
  let end_time = Unix.gettimeofday () in 
  V.console_out V.Info v_lvl (fun () -> "RESULT: " ^ NodeSet.to_string value);
  V.duration_out V.Info v_lvl start_time end_time "";
  value