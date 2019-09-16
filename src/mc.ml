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

let rec model_check lts formula args env v_lvl =

  V.console_out V.Detailed v_lvl (fun () -> "Step: " ^ F.to_string formula);
  match formula with
    F.Const(b) -> if b then begin 
                    let value = Lts.get_all_nodes lts in
                    V.console_out V.Detailed v_lvl (fun () -> " Case: Top - Value: " ^ NodeSet.to_string value);
                    value 
                  end else begin
                    V.console_out V.Detailed v_lvl (fun () -> " Case: Bot - Value: []");
                    NodeSet.empty
                  end

  | F.Prop(prop) -> let value = Lts.get_nodes_of_proposition lts prop in 
                    V.console_out V.Detailed v_lvl (fun () -> " Case: Prop " ^ prop ^ " - Value: " ^ NodeSet.to_string value);
                    value
                    
  | F.Var(var,var_t) -> let current_val = (VarMap.get (F.Var(var, var_t)) env) in
                        if S.is_defined_for_args ~v_lvl current_val args then 
                          (match S.get_value_for_args current_val args with
                            | S.Base(ns) -> ns
                            | S.Fun(map) -> print_endline "Error: Case Var returned fun."; NodeSet.empty)
                        else
                        NodeSet.empty

  | F.Neg(phi) -> let value = NodeSet.diff (Lts.get_all_nodes lts) (model_check lts phi args env v_lvl) in
                  V.console_out V.Detailed v_lvl (fun () -> " Case: Negation of " ^ F.to_string phi ^ " - Value: " ^ NodeSet.to_string value);
                  value

  | F.Conj(phi_1, phi_2) -> let value = NodeSet.inter (model_check lts phi_1 args env v_lvl) (model_check lts phi_2 args env v_lvl) in
                            V.console_out V.Detailed v_lvl (fun () -> " Case: Conj of " 
                                                                        ^ F.to_string phi_1 ^ " and " ^ F.to_string phi_2 
                                                                        ^ " - Value: " ^ NodeSet.to_string value);
                            value

  | F.Disj(phi_1, phi_2) -> let value = NodeSet.union (model_check lts phi_1 args env v_lvl) (model_check lts phi_2 args env v_lvl) in
                            V.console_out V.Detailed v_lvl (fun () -> " Case: Disj of " 
                                                                        ^ F.to_string phi_1 ^ " and " ^ F.to_string phi_2 
                                                                        ^ " - Value: " ^ NodeSet.to_string value);
                            value

  | F.Impl(phi_1, phi_2) -> let value = NodeSet.union (model_check lts (F.Neg(phi_1)) args env v_lvl) (model_check lts phi_2 args env v_lvl) in 
                            V.console_out V.Detailed v_lvl (fun () -> " Case: Impl of " 
                                                                        ^ F.to_string phi_1 ^ " and " ^ F.to_string phi_2 
                                                                        ^ " - Value: " ^ NodeSet.to_string value);
                            value

  | F.Equiv(phi_1, phi_2) ->  let value = NodeSet.inter (model_check lts (F.Impl(phi_1,phi_2)) args env v_lvl) 
                                                        (model_check lts (F.Impl(phi_2,phi_1)) args env v_lvl) in 
                              V.console_out V.Detailed v_lvl (fun () -> " Case: Equiv of " 
                                                                        ^ F.to_string phi_1 ^ " and " ^ F.to_string phi_2 
                                                                        ^ " - Value: " ^ NodeSet.to_string value);
                              value

  | F.Diamond(trans, phi) ->  let val_diamond_phi = ref NodeSet.empty in
                              let val_phi = model_check lts phi args env v_lvl in 
                              NodeSet.iter (fun n -> 
                                              val_diamond_phi := NodeSet.union  !val_diamond_phi
                                                                                (Lts.get_trans_predecessors_of_node lts trans n)) 
                                              val_phi;
                              V.console_out V.Detailed v_lvl (fun () -> " Case: Diamond of " ^ F.to_string phi ^ " - Value: " ^ NodeSet.to_string !val_diamond_phi);
                              !val_diamond_phi 

  | F.Box(transition, phi) -> let value = model_check lts (F.Neg(F.Diamond(transition, F.Neg(phi)))) args env v_lvl in 
                              V.console_out V.Detailed v_lvl (fun () -> " Case: Box of " ^ F.to_string phi ^ " - Value: " ^ NodeSet.to_string value);
                              value

  | F.Mu(var,var_t,phi) -> NodeSet.empty (*TODO*)

  | F.Nu(var,var_t,phi) -> NodeSet.empty (*TODO*)

  | F.Lambda(var,var_t,phi) ->  let value = model_check lts phi (List.tl args) (VarMap.set (F.Var (var,var_t)) (List.nth args 0) env) v_lvl in 
                                V.console_out V.Detailed v_lvl (fun () -> " Case: Lambda " ^ var ^ " of " ^ F.to_string phi ^ " - Value: " ^ NodeSet.to_string value);
                                value

  | F.App(phi_1, phi_2) ->  let phi_2_sem = fully_calc_sem lts phi_2 [] env v_lvl in 
                            let value = model_check lts phi_1 (phi_2_sem :: args) env v_lvl in
                            V.console_out V.Detailed v_lvl (fun () -> " Case: App of " ^ F.to_string phi_2 ^ 
                                                                      " to " ^ F.to_string phi_1 ^ " - Value of Arg: " 
                                                                      ^ S.to_string phi_2_sem ^ " - Value Total: " ^ NodeSet.to_string value);
                            value

and fully_calc_sem lts formula args env v_lvl =
  let rec helper lts formula args env v_lvl = match formula with
   F.Lambda(var, var_t, phi) | F.Mu(var, var_t, phi) -> let all_args = SemanticsSet.all_of_type lts var_t in
                                                        V.console_out V.Debug v_lvl (fun () -> "  Arguments: " ^ SemanticsSet.to_string all_args);
                                                        SemanticsSet.fold (fun arg sem -> S.set_value_for_args ~v_lvl (helper lts phi (arg :: args) env v_lvl) sem [arg]) 
                                                                          all_args (S.empty_fun) 
  | F.Var(var,var_t) -> if VarMap.mem (F.Var(var,var_t)) env then begin
                          V.console_out V.Debug v_lvl (fun () -> "Variable " ^ var ^ " exists in env map."); 
                          VarMap.get (F.Var(var,var_t)) env 
                        end 
                        else begin
                          let argument = List.nth args 0 in
                          let func = List.nth args 1 in (* SHORTLY HACKED *)
                          V.console_out V.Debug v_lvl (fun () -> "Variable " ^ var ^ " does not exist in env map.");
                          Semantics.get_value_for_args func [argument]
                        end
  | F.App(phi_1,phi_2) -> helper lts phi_1 ((helper lts phi_2 [] env v_lvl) :: args) env v_lvl
  | _ -> S.Base(model_check lts formula [] env V.None) in 
  V.console_out V.Detailed v_lvl (fun () -> "Calc: " ^ F.to_string formula ^ " completely...");
  let value = helper lts formula args env v_lvl in
  V.console_out V.Detailed v_lvl (fun () -> "Result: " ^ S.to_string value);
  value

let fully_calc_sem ?(verb_lvl=V.Debug) lts formula =
  fully_calc_sem lts formula [] VarMap.empty verb_lvl

let model_check ?(verb_lvl=V.Info) lts formula =
  V.console_out V.Info verb_lvl (fun () -> "MODEL CHECKER INPUT:");
  V.console_out V.Info verb_lvl (fun () -> "LTS: " ^ Lts.to_string lts);
  V.console_out V.Info verb_lvl (fun () -> "FORMULA: " ^ F.to_string formula);
  V.console_out V.Info verb_lvl (fun () -> "Start model checker...");
  let value = model_check lts formula [] VarMap.empty verb_lvl in 
  V.console_out V.Info verb_lvl (fun () -> "RESULT: " ^ NodeSet.to_string value ^ "\n");
  value