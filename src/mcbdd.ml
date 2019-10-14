open Tcsset
open Hfl
  module F = Formula
module V = Verbose 

open Bddlts

module type VarMapInt = sig 
  type t 
  val empty : t
  val mem : F.t -> t -> bool
  val set : F.t -> HO.hot -> t -> t
  val get : F.t -> t -> HO.hot

end

module VarMap : VarMapInt  = struct (*TODO: Add typecheck between var_t and value and exceptions*)
  (*Simple map, which pairs variable (formula) with its current (semantic) value. 
    Based on TreeMap. (SEE TCSLib)
  *)
  type t = (F.t, HO.hot) TreeMap.t
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
                      (HO.Base (MLBDD.dtrue (Bddlts.get_man lts)), env)
                    else
                      (HO.Base (MLBDD.dfalse (Bddlts.get_man lts)), env)

    | F.Prop(prop) -> (HO.Base(Bddlts.get_prop lts prop),env)

    | F.Var(var,var_t,fp_t) -> 
                        let current_val = (VarMap.get (F.Var(var, var_t,fp_t)) env) in
                        if HO.is_defined_for_args current_val args then begin
                          match HO.get_value_for_args current_val args with
                            HO.Base(ns) -> (HO.Base ns,env)
                          | HO.Fun(map) -> assert false
                        end else begin
                          match fp_t with 
                            F.NoFP -> assert false 
                          | F.LFP ->  assert false
                          | F.GFP ->  assert false
                        end                    
    | F.Neg(phi) -> let (bdd,env) = model_check' phi lts args env v_lvl ind in
                    (match bdd with HO.Base bdd -> (HO.Base(MLBDD.dnot bdd), env) | _ -> assert false)

    | F.Conj(phi1, phi2) -> let (bdd1,env1) = model_check' phi1 lts args env v_lvl ind in
                            let (bdd2,env2) = model_check' phi2 lts args env1 v_lvl ind in
                            (match (bdd1,bdd2) with 
                              (HO.Base bdd1,HO.Base bdd2) -> (HO.Base(MLBDD.dand bdd1 bdd2), env) 
                            | _ -> assert false)

    | F.Disj(phi1, phi2) -> let (bdd1,env1) = model_check' phi1 lts args env v_lvl ind in
                            let (bdd2,env2) = model_check' phi2 lts args env1 v_lvl ind in
                            (match (bdd1,bdd2) with 
                              (HO.Base bdd1,HO.Base bdd2) -> (HO.Base(MLBDD.dor bdd1 bdd2), env) 
                            | _ -> assert false)
    | F.Impl(phi1, phi2) -> let (bdd1,env1) = model_check' phi1 lts args env v_lvl ind in
                            let (bdd2,env2) = model_check' phi2 lts args env1 v_lvl ind in
                            (match (bdd1,bdd2) with 
                              (HO.Base bdd1,HO.Base bdd2) -> (HO.Base(MLBDD.imply bdd1 bdd2), env) 
                            | _ -> assert false)
    | F.Equiv(phi1, phi2) ->  let (bdd1,env1) = model_check' phi1 lts args env v_lvl ind in
                              let (bdd2,env2) = model_check' phi2 lts args env1 v_lvl ind in
                              (match (bdd1,bdd2) with 
                              (HO.Base bdd1,HO.Base bdd2) -> (HO.Base(MLBDD.eq bdd1 bdd2), env) 
                            | _ -> assert false)
    | F.Diamond(trans, phi) ->  let (bdd,env) = model_check' phi lts args env v_lvl ind in
                                (match bdd with
                                  HO.Base bdd -> (
                                                HO.Base
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
                                HO.Base  bdd -> (
                                          HO.Base
                                          (MLBDD.forall 
                                            (Bddlts.get_tovars_support lts)
                                            (MLBDD.dor 
                                              (MLBDD.dnot (Bddlts.get_trans lts trans)) 
                                              (MLBDD.permute (Bddlts.get_tovars_ids_as_array lts) bdd)
                                            )), (*TODO encapsulate into bddlts*)
                                          env
                                        )
                              | _ -> assert false)
  | F.Mu(var,var_t,phi) ->  (*let x = F.Var(var,var_t,LFP) in
                            let rec repeat_until map =
                              (*repeat until approximation is found (i.e. fixpoint) 
                                returns env including fp approximation for x.
                              *)
                              let start_time = Unix.gettimeofday () in
                              let update = ref 
                                (match var_t with 
                                  F.Base -> HO.empty_base lts | _ -> HO.empty_fun lts) 
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
                            | S.Fun(map)-> assert false)*)(HO.Base(Bddlts.get_prop lts "p"),env)


  | F.Nu(var,var_t,phi) -> (HO.Base(Bddlts.get_prop lts "p"),env)

  | F.Lambda(var,var_t,phi) -> model_check' phi lts (List.tl args) (VarMap.set (F.Var(var,var_t,F.NoFP)) (List.hd args) env) v_lvl ind

  | F.App(phi1, phi2) ->    let phi_2_sem = fully_calc phi2 lts [] env v_lvl indent in 
                            let (value, env) = model_check' phi1 lts (phi_2_sem :: args) env v_lvl (indent ^ "  ") in
                            (value,env)
  and fully_calc formula lts args env v_lvl indent =
  let rec helper formula lts args env v_lvl = 
    match formula with
      F.Lambda(var, var_t, phi)  ->   let all_args = HO.all_of_type lts var_t in
                                      List.fold_left
                                        (fun sem arg ->  
                                                        HO.set_value_for_args
                                                          (helper phi lts (args) 
                                                            (VarMap.set (F.Var(var,var_t,F.NoFP)) arg env) v_lvl
                                                          ) 
                                                          sem 
                                                          [arg]
                                        ) 
                                        (HO.empty_fun) all_args 
                                    
    | F.Mu(var, var_t, phi) ->  let (value,_) = model_check' 
                                                (F.Mu(var, var_t, phi)) 
                                                lts (args) 
                                                env V.None ("  " ^ indent)
                                in value
                                (*TODO: only fp arguments of type * -> base are possible with this *)
                                (*TODO: GFP?!*)
                                  
    | F.Var(var,var_t,fp_t) ->  if VarMap.mem (F.Var(var,var_t,fp_t)) env then begin
                                  HO.get_value_for_args (VarMap.get (F.Var(var,var_t,fp_t)) env) args end
                                else assert false

    | F.App(phi_1,phi_2) -> let (value,_) = 
                              model_check' phi_1 lts ((helper phi_2 lts [] env v_lvl) :: args) env V.None (indent ^ "  ")
                            in value

    | _ -> let (value,_) = model_check' formula lts [] env V.None (indent ^ "  ") in value
  in
  let start_time = Unix.gettimeofday() in
  let value = helper formula lts args env v_lvl in
  let end_time = Unix.gettimeofday() in
  value
  in

  let (bdd,_) = model_check' formula lts arguments environment v_lvl indent in
  match bdd with Base bdd -> Bddlts.get_all_sat_states lts bdd | _ -> assert false

let model_check ?(v_lvl=V.Detailed) formula lts =
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