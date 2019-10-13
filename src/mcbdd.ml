open Tcsset
open Hfl
  module F = Formula
module V = Verbose 
type ho_t = 
    Base of MLBDD.t 
  | Fun of (ho_t -> ho_t)

let rec apply_arg_list hot arglist = (*This gives me goosebumps*)
  match (hot,arglist) with 
    (Base bdd, []) -> hot
  | (Fun f, h::t) -> apply_arg_list (f h) t
  | (Fun f, []) -> hot
  | _ -> assert false

module type VarMapInt = sig 
  type t 
  val empty : t
  val mem : F.t -> t -> bool
  val set : F.t -> ho_t -> t -> t
  val get : F.t -> t -> ho_t

end

module VarMap : VarMapInt  = struct (*TODO: Add typecheck between var_t and value and exceptions*)
  (*Simple map, which pairs variable (formula) with its current (semantic) value. 
    Based on TreeMap. (SEE TCSLib)
  *)
  type t = (F.t, ho_t) TreeMap.t
    (* varmap type. (key:variable, value: semantic object) *)

  let empty = TreeMap.empty compare
    (* creates empty treemap *)

  let mem = TreeMap.mem
    (*check if variable is defined in map *)

  let set argument value map = 
    (* sets / initializes value for given argument*)
    match argument with
      F.Var(var_name,var_t,fp_t) ->  TreeMap.add argument value map  
    | _ ->  assert false 

  let get argument map =
    (* Returns current value for given argument. safetychecks included*)
    match argument with
      F.Var(var_name,var_t,fp_t) -> if mem argument map then
                                      TreeMap.find argument map
                                    else 
                                      assert false 
    | _ ->  assert false
end

let model_check formula lts arguments environment v_lvl indent =
  let rec model_check' form lts args env v_lvl ind =
    match form with
      F.Const(b) -> if b then 
                      (Base (MLBDD.dtrue (Bddlts.get_man lts)), env)
                    else
                      (Base(MLBDD.dfalse (Bddlts.get_man lts)), env)

    | F.Prop(prop) -> (Base(Bddlts.get_prop lts prop),env)

    | F.Var(var,var_t,fp_t) ->  let current_val = (VarMap.get (F.Var(var, var_t,fp_t)) env) in
                                (match fp_t with 
                                  F.NoFP -> (match current_val with
                                              Base bdd -> (Base bdd,env)
                                            | Fun f -> (apply_arg_list (Fun f) args, env) 
                                            )
                                | F.LFP -> assert false
                                | F.GFP -> assert false
                                )                      
    | F.Neg(phi) -> let (bdd,env) = model_check' phi lts args env v_lvl ind in
                    (match bdd with Base bdd -> (Base(MLBDD.dnot bdd), env) | _ -> assert false)

    | F.Conj(phi1, phi2) -> let (bdd1,env1) = model_check' phi1 lts args env v_lvl ind in
                            let (bdd2,env2) = model_check' phi2 lts args env1 v_lvl ind in
                            (match (bdd1,bdd2) with 
                              (Base bdd1,Base bdd2) -> (Base(MLBDD.dand bdd1 bdd2), env) 
                            | _ -> assert false)

    | F.Disj(phi1, phi2) -> let (bdd1,env1) = model_check' phi1 lts args env v_lvl ind in
                            let (bdd2,env2) = model_check' phi2 lts args env1 v_lvl ind in
                            (match (bdd1,bdd2) with 
                              (Base bdd1,Base bdd2) -> (Base(MLBDD.dor bdd1 bdd2), env) 
                            | _ -> assert false)
    | F.Impl(phi1, phi2) -> let (bdd1,env1) = model_check' phi1 lts args env v_lvl ind in
                            let (bdd2,env2) = model_check' phi2 lts args env1 v_lvl ind in
                            (match (bdd1,bdd2) with 
                              (Base bdd1,Base bdd2) -> (Base(MLBDD.imply bdd1 bdd2), env) 
                            | _ -> assert false)
    | F.Equiv(phi1, phi2) ->  let (bdd1,env1) = model_check' phi1 lts args env v_lvl ind in
                              let (bdd2,env2) = model_check' phi2 lts args env1 v_lvl ind in
                              (match (bdd1,bdd2) with 
                              (Base bdd1,Base bdd2) -> (Base(MLBDD.eq bdd1 bdd2), env) 
                            | _ -> assert false)
    | F.Diamond(trans, phi) ->  let (bdd,env) = model_check' phi lts args env v_lvl ind in
                                (match bdd with
                                  Base bdd -> (
                                                Base
                                                (MLBDD.exists 
                                                  (Bddlts.get_tovars_support lts)
                                                  (MLBDD.dand 
                                                    (Bddlts.get_trans lts trans) 
                                                    (MLBDD.permute (Bddlts.get_tovars_ids_as_array lts) bdd)
                                                  )), (*TODO encapsulate into bddlts*)
                                                env
                                              )
                                | _ -> assert false)
    | F.Box(trans, phi) ->  (let (bdd,env) = model_check' phi lts args env v_lvl ind in
                              match bdd with 
                                Base  bdd -> (
                                          Base
                                          (MLBDD.forall 
                                            (Bddlts.get_tovars_support lts)
                                            (MLBDD.dor 
                                              (MLBDD.dnot (Bddlts.get_trans lts trans)) 
                                              (MLBDD.permute (Bddlts.get_tovars_ids_as_array lts) bdd)
                                            )), (*TODO encapsulate into bddlts*)
                                          env
                                        )
                              | _ -> assert false)
  | F.Mu(var,var_t,phi) -> (Base(Bddlts.get_prop lts "p"),env)
  | F.Nu(var,var_t,phi) -> (Base(Bddlts.get_prop lts "p"),env)

  | F.Lambda(var,var_t,phi) ->  (match args with 
                                  [] -> (Fun  (fun arg -> 
                                                let (bdd, env) = 
                                                  model_check' phi lts [] (VarMap.set (F.Var(var,var_t,F.NoFP)) arg env) v_lvl ind 
                                                in bdd
                                              )
                                        ,env) (*Could be a problem with the env type because of later eval *)
                                | h :: t -> model_check' phi lts t (VarMap.set (F.Var(var,var_t,F.NoFP)) h env) v_lvl ind)

  | F.App(phi1, phi2) ->  let rec check_for_fp = function
                            F.Mu (_,_,_) | F.Nu (_,_,_) -> true
                          | F.App (f,a) -> check_for_fp f 
                          | _ -> false
                          in 
                            if true then
                              let (phi2_sem, phi2_env) = model_check' phi2 lts [] env v_lvl ind in 
                              model_check' phi1 lts (phi2_sem :: args) phi2_env v_lvl ind 
                            else
                              (Base(Bddlts.get_prop lts "p"),env)

  and
  fully_calc form lts args env =
    (Base(Bddlts.get_prop lts "p"),env)
  in
  let (bdd,_) = model_check' formula lts arguments environment v_lvl indent in
  match bdd with Base bdd -> Bddlts.get_all_sat_states lts bdd | _ -> assert false

let model_check ?(v_lvl=V.Info) formula lts =
  V.console_out V.Info v_lvl (fun () -> "MODEL CHECKER INPUT:");
  V.console_out V.Info v_lvl (fun () -> Bddlts.to_string lts ^"\n");
  V.console_out V.Info v_lvl (fun () -> "FORMULA: " ^ F.to_string ~show_types:false formula ^ "\n\n");
  V.console_out V.Info v_lvl (fun () -> "Start model checker...");
  let start_time = Unix.gettimeofday () in
  let value = model_check formula lts [] VarMap.empty v_lvl "  " in
  let end_time = Unix.gettimeofday () in 
  V.console_out V.Info v_lvl (fun () -> "RESULT: " ^ Tools.list_to_string value string_of_int);
  V.duration_out V.Info v_lvl start_time end_time "";
  value