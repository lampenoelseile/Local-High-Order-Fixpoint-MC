open Hfl
  module F = Formula
module V = Verbose 

(*
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
end*)

module Semantics = struct

end
let model_check formula lts arguments environment v_lvl indent =
  let rec model_check' form lts args env v_lvl ind =
    match form with
      F.Const(b) -> if b then 
                      (MLBDD.dtrue (Bddlts.get_man lts), env)
                    else
                      (MLBDD.dfalse (Bddlts.get_man lts), env)
    | F.Prop(prop) -> (Bddlts.get_prop lts prop,environment)
| F.Var(var,var_t,fp_t) -> (Bddlts.get_prop lts "p",environment)
    | F.Neg(phi) -> let (bdd,env) = model_check' phi lts args env v_lvl ind in
                    (MLBDD.dnot bdd, env)
    | F.Conj(phi1, phi2) -> let (bdd1,env1) = model_check' phi1 lts args env v_lvl ind in
                            let (bdd2,env2) = model_check' phi2 lts args env1 v_lvl ind in
                            (MLBDD.dand bdd1 bdd2, env)
    | F.Disj(phi1, phi2) -> let (bdd1,env1) = model_check' phi1 lts args env v_lvl ind in
                            let (bdd2,env2) = model_check' phi2 lts args env1 v_lvl ind in
                            (MLBDD.dor bdd1 bdd2, env)
    | F.Impl(phi1, phi2) -> let (bdd1,env1) = model_check' phi1 lts args env v_lvl ind in
                            let (bdd2,env2) = model_check' phi2 lts args env1 v_lvl ind in
                            (MLBDD.imply bdd1 bdd2, env)
    | F.Equiv(phi1, phi2) ->  let (bdd1,env1) = model_check' phi1 lts args env v_lvl ind in
                              let (bdd2,env2) = model_check' phi2 lts args env1 v_lvl ind in
                              (MLBDD.eq bdd1 bdd2, env)
    | F.Diamond(trans, phi) ->  let (bdd,env) = model_check' phi lts args env v_lvl ind in
                                (
                                  MLBDD.exists 
                                    (Bddlts.get_tovars_support lts)
                                    (MLBDD.dand 
                                      (Bddlts.get_trans lts trans) 
                                      (MLBDD.permute (Bddlts.get_tovars_ids_as_array lts) bdd)
                                    ), (*TODO encapsulate into bddlts*)
                                  environment
                                )
    | F.Box(trans, phi) -> let (bdd,env) = model_check' phi lts args env v_lvl ind in
                                (
                                  MLBDD.forall 
                                    (Bddlts.get_tovars_support lts)
                                    (MLBDD.dand 
                                      (Bddlts.get_trans lts trans) 
                                      (MLBDD.permute (Bddlts.get_tovars_ids_as_array lts) bdd)
                                    ), (*TODO encapsulate into bddlts*)
                                  environment
                                )
  | F.Mu(var,var_t,phi) -> (Bddlts.get_prop lts "p",environment)
  | F.Nu(var,var_t,phi) -> (Bddlts.get_prop lts "p",environment) 
  | F.Lambda(var,var_t,phi) -> (Bddlts.get_prop lts "p",environment)
  | F.App(phi1, phi2) -> (Bddlts.get_prop lts "p",environment)
  in 
  let (bdd,_) = model_check' formula lts arguments environment v_lvl indent in
  Bddlts.get_sat_states lts bdd

let model_check ?(v_lvl=V.Info) formula lts =
  V.console_out V.Info v_lvl (fun () -> "MODEL CHECKER INPUT:");
  V.console_out V.Info v_lvl (fun () -> "LTS: " ^ Bddlts.to_string lts ^"\n");
  V.console_out V.Info v_lvl (fun () -> "FORMULA: " ^ F.to_string formula ^ "\n\n");
  V.console_out V.Info v_lvl (fun () -> "Start model checker...");
  let start_time = Unix.gettimeofday () in
  let value = model_check formula lts [] () v_lvl "  " in
  let end_time = Unix.gettimeofday () in 
  V.console_out V.Info v_lvl (fun () -> "RESULT: " ^ Tools.list_to_string value string_of_int);
  V.duration_out V.Info v_lvl start_time end_time "";
  value