open Datastructures
open Hfl
  module F = Formula
  module S = Semantics

module V = Verbose
open Tcsset

module type VarMapInt = sig
  (*interface: VarMap module. 
    Represents mapping from a Variable (see formula.t)
  *)
  type t

  val empty : t
  val set : F.t -> S.t -> t -> t
  val get : F.t -> t -> S.t
  val mem : F.t -> t -> bool
end

module VarMap : VarMapInt = struct

  type t = (F.t, S.t) TreeMap.t

  let empty = TreeMap.empty compare

  let set variable value map = 
    match variable with
      F.Var(var_name,var_t) -> (*TODO: Add typecheck.*) TreeMap.add variable value map
    | _ -> print_endline "Error. Given Key is not a variable."; map
  
  let get variable map =
    if TreeMap.mem variable map then
      TreeMap.find variable map
    else
      assert false

  let mem = TreeMap.mem
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
  | F.Mu(var,var_t,phi) -> NodeSet.empty 
  | F.Nu(var,var_t,phi) -> NodeSet.empty
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
  | F.Var(var,var_t) -> if VarMap.mem (Var(var,var_t)) env then begin
                          V.console_out V.Debug v_lvl (fun () -> "Variable " ^ var ^ " exists in env map."); 
                          VarMap.get (Var(var,var_t)) env       (*TODO: THIS DOES NOT WORK. ARGUMENT 0 is not value*)
                        end 
                        else begin
                          let argument = List.nth args 0 in
                          let func = List.nth args 1 in
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