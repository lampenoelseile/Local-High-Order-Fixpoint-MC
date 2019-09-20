open Datastructures
open Hfl
  module F = Formula
  module S = Semantics

module V = Verbose
open Tcsset


module VarMap  = struct
  (*Variable map. Simple map, which pairs variable with its current semantic value. 
    Based on TreeMap. (SEE TCSLib)
  *)
  type t = (F.t, S.t) TreeMap.t
    (* varmap type. (key:variable, value: semantic object) *)
  let empty = TreeMap.empty compare
    (* creates empty treemap *)
  let mem = TreeMap.mem
    (*check if variable is defined in mem *)
  let set ?(v_lvl=V.None) argument value map = 
    (* sets / initializes value for given argument*)
    match argument with
      F.Var(var_name,var_t) ->  TreeMap.add argument value map  (*TODO: Add typecheck between var_t and value *)
    | _ ->  Verbose.console_out V.Debug v_lvl 
              (fun () -> "VarMap argument was not of type Var(...)"); assert false 
  let get ?(v_lvl=V.None) argument map =
    (* gets current value for given argument. safetychecks included*)
    match argument with
      F.Var(var_name,var_t) ->  if mem argument map then
                                  TreeMap.find argument map
                                else
                                begin
                                  Verbose.console_out V.Debug v_lvl 
                                    (fun () -> "VarMap argument was undefined, but value was asked for");
                                  assert false
                                end
    | _ ->  Verbose.console_out V.Debug v_lvl 
              (fun () -> "VarMap argument was not of type Var(...)"); 
            assert false
end

let rec model_check formula lts args env v_lvl indent =
  (* lazy model checker. *)
  match formula with
    F.Const(b) -> if b then begin 
                    let value = Lts.get_all_nodes lts in
                    V.console_out V.Detailed v_lvl 
                      (fun () -> indent ^ "  Case: Top - Value: " ^ NodeSet.to_string value);
                    (value,env)
                  end else begin
                    V.console_out V.Detailed v_lvl (fun () -> indent ^ "  " ^ "Case: Bot - Value: []");
                    (NodeSet.empty,env)
                  end

  | F.Prop(prop) -> let value = Lts.get_nodes_of_proposition lts prop in 
                    V.console_out V.Detailed v_lvl 
                      (fun () -> indent ^ "  " ^ "Case: Prop " ^ prop ^ " - Value: " ^ NodeSet.to_string value);
                    (value,env)
                    
  | F.Var(var,var_t) -> V.console_out V.Detailed v_lvl 
                          (fun () -> indent ^ "  " ^ "Case: Var " ^ var);
                        let current_val = (VarMap.get (F.Var(var, var_t)) env) in
                        if S.is_defined_for_args current_val args then
                          begin 
                            match S.get_value_for_args current_val args with
                              S.Base(ns) -> V.console_out V.Detailed v_lvl 
                                              (fun () -> indent ^ "  " ^ "Value: " ^ NodeSet.to_string ns);(ns,env)
                            | S.Fun(map) -> assert false
                          end
                        else
                        (NodeSet.empty, VarMap.set ~v_lvl (Var (var,var_t)) (S.set_value_for_args (Base NodeSet.empty) current_val args) env)

  | F.Neg(phi) -> V.console_out V.Detailed v_lvl 
                    (fun () -> indent ^ "  " ^ "Case: Negation of " ^ F.to_string phi);
                  let (val1, env) = (model_check phi lts args env v_lvl(indent ^ "  ")) in
                  let value = NodeSet.diff (Lts.get_all_nodes lts) val1
                  in
                  V.console_out V.Detailed v_lvl 
                    (fun () -> indent ^ "  " ^ "Value: " ^ NodeSet.to_string value);
                  (value,env)

  | F.Conj(phi_1, phi_2) -> V.console_out V.Detailed v_lvl 
                              (fun () ->  indent ^ "  " ^"Case: Conj of " ^ F.to_string phi_1 
                                          ^ " and " ^ F.to_string phi_2);
                            let (val1, env) = model_check phi_1 lts args env v_lvl (indent ^ "  ") in 
                            let (val2, env) = model_check phi_2 lts args env v_lvl (indent ^ "  ") in
                            let value = NodeSet.inter val1 val2
                            in
                            V.console_out V.Detailed v_lvl 
                              (fun () ->  indent ^ "  " ^"Value: " ^ NodeSet.to_string value);
                            (value,env)

  | F.Disj(phi_1, phi_2) -> V.console_out V.Detailed v_lvl 
                              (fun () ->  indent ^ "  " ^ "Case: Disj of " ^ F.to_string phi_1 ^ " and " ^ F.to_string phi_2);
                            let (val1, env) = model_check phi_1 lts args env v_lvl (indent ^ "  ") in 
                            let (val2, env) = model_check phi_2 lts args env v_lvl (indent ^ "  ") in
                            let value = NodeSet.union val1 val2
                            in
                            V.console_out V.Detailed v_lvl 
                              (fun () ->  indent ^ "  " ^ "Value: " ^ NodeSet.to_string value);
                            (value,env)

  | F.Impl(phi_1, phi_2) -> V.console_out V.Detailed v_lvl 
                              (fun () ->  indent ^ "  " ^ "Case: Impl of " ^ F.to_string phi_1 ^ " and " ^ F.to_string phi_2);
                            let (val1, env) = model_check (F.Neg(phi_1)) lts args env v_lvl (indent ^ "  ")in 
                            let (val2, env) = model_check phi_2 lts args env v_lvl (indent ^ "  ") in
                            let value = NodeSet.union val1 val2
                            in 
                            V.console_out V.Detailed v_lvl 
                              (fun () ->  indent ^ "  " ^ "Value: " ^ NodeSet.to_string value);
                            (value,env)

  | F.Equiv(phi_1, phi_2) ->  V.console_out V.Detailed v_lvl 
                                (fun () ->  indent ^ "  " ^ "Case: Equiv of " ^ F.to_string phi_1 ^ " and " ^ F.to_string phi_2);
                              let (val1, env) = model_check (F.Impl(phi_1,phi_2)) lts args env v_lvl (indent ^ "  ") in 
                              let (val2, env) = model_check (F.Impl(phi_2,phi_1)) lts args env v_lvl (indent ^ "  ") in
                              let value = NodeSet.inter  val1 val2                  
                              in 
                              V.console_out V.Detailed v_lvl 
                                (fun () ->  indent ^ "  " ^ "Value: " ^ NodeSet.to_string value);
                              (value, env)

  | F.Diamond(trans, phi) ->  V.console_out V.Detailed v_lvl 
                                (fun () ->  indent ^ "  " ^ "Case: Diamond " ^ trans ^ " of " ^ F.to_string phi);
                              let val_diamond_phi = ref NodeSet.empty in
                              let (val_phi, env) = model_check phi lts args env v_lvl (indent ^ "  ") in 
                              NodeSet.iter (fun n -> 
                                              val_diamond_phi :=  NodeSet.union  !val_diamond_phi
                                                                    (Lts.get_trans_predecessors_of_node lts trans n)) 
                                            val_phi;
                              V.console_out V.Detailed v_lvl 
                                (fun () ->  indent ^ "  " ^ "Value: " ^ NodeSet.to_string !val_diamond_phi);
                              (!val_diamond_phi, env)

  | F.Box(trans, phi) -> V.console_out V.Detailed v_lvl 
                                (fun () -> indent ^ "  " ^ "Case: Box " ^ trans ^ " of " ^ F.to_string phi);
                              let (value,env) = model_check (F.Neg(F.Diamond(trans, F.Neg(phi)))) lts args env v_lvl  (indent ^ "  ") in 
                              V.console_out V.Detailed v_lvl 
                                (fun () -> indent ^ "  " ^ "Value: " ^ NodeSet.to_string value);
                              (value,env)

  | F.Mu(var,var_t,phi) ->  V.console_out V.Detailed v_lvl 
                                (fun () -> indent ^ "  " ^ "Case: LFP of " ^ F.to_string phi 
                                            ^ " regarding variable " ^ var);
                            let x = F.Var(var,var_t) in
                            let rec repeat_until map =
                              let update = ref S.empty_fun in
                              let defined_args = (S.get_defined_arguments (VarMap.get ~v_lvl x map)) in
                              List.iter (fun arg_list ->
                                          V.console_out V.Detailed v_lvl 
                                            (fun () -> indent ^ "  "^ "LFP Arg: [" 
                                              ^ List.fold_left (fun str arg -> S.to_string arg) "" arg_list ^ "]"); 
                                          let (value, map_tmp) = model_check phi lts arg_list map v_lvl (indent ^ "   ") in
                                          let deff_arg = S.get_defined_arguments (VarMap.get ~v_lvl x map_tmp) in
                                          List.iter (fun arg_list -> update := S.set_value_for_args 
                                                      (S.get_value_for_args (VarMap.get ~v_lvl x map_tmp) arg_list) !update arg_list) 
                                          (List.filter (fun x -> not (List.mem x defined_args)) deff_arg);
                                          update := S.set_value_for_args 
                                                      (S.Base value) !update arg_list) defined_args;
                              if S.compare !update (VarMap.get ~v_lvl x map) == 0 then
                              begin
                                V.console_out V.Detailed v_lvl (fun () -> indent ^ "  " ^ "FP found."); 
                                map
                              end
                              else
                              begin
                                V.console_out V.Detailed v_lvl (fun () -> indent ^ "  "^ "Approximation changed."); 
                                repeat_until (VarMap.set ~v_lvl x !update map)
                              end
                            in
                            let initialized_map = VarMap.set ~v_lvl x (S.set_value_for_args S.empty_base S.empty_fun args) env in
                            let approx = VarMap.get ~v_lvl x (repeat_until initialized_map) in
                            V.sem_log_out V.Detailed v_lvl approx (F.Mu(var,var_t,phi));
                            (match S.get_value_for_args approx args with
                              S.Base ns -> (ns,env) (*TODO: detailed output *)
                            | S.Fun(map)-> assert false)
                            
  | F.Nu(var,var_t,phi) ->  let (value, env) = model_check (F.nu_to_mu (F.Nu(var,var_t,phi))) lts args env v_lvl (indent ^ "  ") in 
                            (value,env) (*TODO: detailed output *)

  | F.Lambda(var,var_t,phi) ->  V.console_out V.Detailed v_lvl 
                                  (fun () ->  indent ^ "  " ^ "Case: Lambda " ^ var ^ " of " 
                                              ^ F.to_string phi );
                                let (value,env) = model_check phi lts (List.tl args) (VarMap.set (F.Var (var,var_t)) 
                                              (List.nth args 0) env) v_lvl (indent ^ "  ")
                                in 
                                V.console_out V.Detailed v_lvl 
                                  (fun () ->  indent ^ "  " ^ "Value: " ^ NodeSet.to_string value);
                                (value,env)

  | F.App(phi_1, phi_2) ->  V.console_out V.Detailed v_lvl 
                              (fun () ->  indent ^ "  " ^ "Case: App of " ^ F.to_string phi_2 ^ " to " 
                                          ^ F.to_string phi_1);
                            let phi_2_sem = fully_calc_sem phi_2 lts [] env v_lvl (indent ^ "  ") in 
                            let (value, env) = model_check phi_1 lts (phi_2_sem :: args) env v_lvl (indent ^ "  ") in
                            V.console_out V.Detailed v_lvl 
                              (fun () ->  indent ^ "  " ^ "Value of Arg: " 
                                          ^ S.to_string phi_2_sem ^ "\n"^ indent ^ "  " ^ "Value Total: " 
                                          ^ NodeSet.to_string value);
                            (value,env)

and fully_calc_sem formula lts args env v_lvl indent =
  let rec helper formula lts args env v_lvl = 
    match formula with
      F.Lambda(var, var_t, phi)  ->  let all_args = SemanticsSet.all_of_type lts var_t in
                                      V.console_out V.Debug v_lvl 
                                    (fun () -> "  Arguments: " ^ SemanticsSet.to_string all_args);
                                  SemanticsSet.fold 
                                    (fun arg sem -> S.set_value_for_args
                                                      (helper phi lts (args) (VarMap.set ~v_lvl (F.Var(var,var_t)) arg env) v_lvl) 
                                                      sem [arg]) 
                                    all_args (S.empty_fun) 
                                    
    | F.Mu(var, var_t, phi) ->  let all_args = SemanticsSet.all_of_type lts var_t in
                                  V.console_out V.Debug v_lvl 
                                    (fun () -> "  Arguments: " ^ SemanticsSet.to_string all_args);
                                  SemanticsSet.fold 
                                    (fun arg sem -> S.set_value_for_args
                                                      (helper phi lts (args) (VarMap.set ~v_lvl (F.Var(var,var_t)) arg env) v_lvl) 
                                                      sem [arg]) 
                                    all_args (S.empty_fun)

    | F.Var(var,var_t) ->  if VarMap.mem (F.Var(var,var_t)) env then 
                            begin
                              V.console_out V.Debug v_lvl 
                                (fun () -> "Variable " ^ var ^ " exists in env map."); 
                              VarMap.get (F.Var(var,var_t)) env 
                            end 
                          else 
                            assert false

    | F.App(phi_1,phi_2) -> helper phi_1 lts ((helper phi_2 lts [] env v_lvl) :: args) env v_lvl

    | _ -> let (value,_) = model_check formula lts [] env v_lvl (indent ^ "  ") in S.Base(value)
  in 
  V.console_out V.Detailed v_lvl (fun () -> indent ^ "  " ^ "Calculate: " ^ F.to_string formula ^ " completely...");
  let value = helper formula lts args env v_lvl in
  V.console_out V.Detailed v_lvl (fun () -> indent ^ "  " ^ "Value: see ./outputs/" ^ F.to_string formula ^ ".log");
  V.sem_log_out V.Detailed v_lvl value formula;
  value

let fully_calc_sem ?(v_lvl=V.Debug) formula lts =
  fully_calc_sem formula lts [] VarMap.empty v_lvl "  "

let model_check ?(v_lvl=V.Info) formula lts =
  V.console_out V.Info v_lvl (fun () -> "MODEL CHECKER INPUT:");
  V.console_out V.Info v_lvl (fun () -> "LTS: " ^ Lts.to_string lts ^"\n");
  V.console_out V.Info v_lvl (fun () -> "FORMULA: " ^ F.to_string formula ^ "\n\n");
  V.console_out V.Info v_lvl (fun () -> "Start model checker...");
  let (value,_) = model_check formula lts [] VarMap.empty v_lvl "" in 
  V.console_out V.Info v_lvl (fun () -> "RESULT: " ^ NodeSet.to_string value);
  value