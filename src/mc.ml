open Datastructures
open Lts
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
end

module VarMap : VarMapInt = struct

  type t = (F.t, S.t) TreeMap.t

  let empty = TreeMap.empty compare

  let set variable value map = 
    match variable with
    | F.Var(var_name,var_t) -> (*TODO: Add typecheck.*) TreeMap.add variable value map
    | _ -> print_endline "Error. Given Key is not a variable."; map
  
  let get variable map =
    TreeMap.find variable map
end

let rec model_check lts formula args env v_lvl =
  match formula with
  | F.Const(b) -> if b then Lts.get_all_nodes lts else NodeSet.empty
  | F.Prop(prop) -> Lts.get_nodes_of_proposition lts prop
  | F.Var(var,var_t) -> let current_val = (VarMap.get (F.Var(var, var_t)) env) in
                        if S.is_defined_for_args current_val args then 
                          (match S.get_value_for_args current_val args with
                            | S.Base(ns) -> ns
                            | S.Fun(map) -> print_endline "Error."; NodeSet.empty)
                        else
                        NodeSet.empty
  | F.Neg(phi) -> NodeSet.diff (Lts.get_all_nodes lts) (model_check lts phi args env v_lvl)
  | F.Conj(phi_1, phi_2) -> NodeSet.inter (model_check lts phi_1 args env v_lvl) (model_check lts phi_2 args env v_lvl)
  | F.Disj(phi_1, phi_2) -> NodeSet.union (model_check lts phi_1 args env v_lvl) (model_check lts phi_2 args env v_lvl)
  | F.Impl(phi_1, phi_2) -> NodeSet.union (model_check lts (F.Neg(phi_1)) args env v_lvl) (model_check lts phi_2 args env v_lvl)
  | F.Equiv(phi_1, phi_2) -> NodeSet.inter (model_check lts (F.Impl(phi_1,phi_2)) args env v_lvl) (model_check lts (F.Impl(phi_2,phi_1)) args env v_lvl)
  | F.Diamond(transition, phi) -> let val_diamond_phi = ref NodeSet.empty in
                                let val_phi = model_check lts phi args env v_lvl in 
                                NodeSet.iter (fun n -> 
                                                val_diamond_phi := NodeSet.union  !val_diamond_phi
                                                                                  (Lts.get_trans_predecessors_of_node lts transition n)) 
                                                val_phi;
                                !val_diamond_phi 
  | F.Box(transition, phi) -> model_check lts (F.Neg(F.Diamond(transition, F.Neg(phi)))) args env v_lvl
  | F.Mu(var,var_t,phi) -> NodeSet.empty 
  | F.Nu(var,var_t,phi) -> NodeSet.empty
  | F.Lambda(var,var_t,phi) -> model_check lts phi (List.tl args) (VarMap.set (F.Var (var,var_t)) (List.nth args 0) env) v_lvl
  | F.App(phi_1, phi_2) -> let phi_2_sem = fully_calc_sem lts phi_2 env in model_check lts phi_2 (phi_2_sem :: args) env v_lvl

and fully_calc_sem lts formula env =
  S.Base(NodeSet.empty)

let model_check ?(verb_lvl=V.Info) lts formula =
  V.console_out V.Info verb_lvl ("Start model checker...");
  V.console_out V.Info verb_lvl ("LTS: " ^ Lts.to_string lts);
  V.console_out V.Info verb_lvl ("FORMULA: " ^ F.to_string formula);
  let value = model_check lts formula [] VarMap.empty verb_lvl in 
  V.console_out V.Info verb_lvl ("RESULT: " ^ NodeSet.to_string value ^ "\n");
  value