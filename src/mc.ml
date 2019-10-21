open Tcsset
open Hfl
  module F = Formula
module S = Semantics
module V = Verbose 

module VarMap = struct    (*TODO: Add typecheck between var_t and value and exceptions*)
  (*Simple map, which pairs variable (formula) with its current (semantic) value. 
    Based on TreeMap. (SEE TCSLib)
  *)
  type t = (F.t, S.t) TreeMap.t
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

let model_check formula lts arguments environment v_lvl indent lazzyfp =
  let rec model_check' form lts args env v_lvl ind lazzfp =
    match form with
      F.Const(b) -> if b then
                      let value = S.full_base lts in
                      V.console_out V.Detailed v_lvl 
                        (fun () -> ind ^ "Case: Top - Value: " ^ S.to_string value);
                      (value, env)
                    else
                      let value = S. empty_base lts in 
                      V.console_out V.Detailed v_lvl 
                      (fun () -> ind ^ "Case: Top - Value: " ^ S.to_string value);
                      (value,env)

    | F.Prop(prop) -> let value = S.Base(Bddlts.get_prop lts prop) in
                      V.console_out V.Detailed v_lvl 
                        (fun () -> ind ^ "Case: Prop " ^ prop ^ " - Value: " ^ S.to_string value);
                      (S.Base(Bddlts.get_prop lts prop),env)

    | F.Var(var,var_t,fp_t) ->  V.console_out V.Detailed v_lvl 
                                  (fun () -> ind ^ "Case: Var " ^ var);
                                let current_val = 
                                      (VarMap.get (F.Var(var, var_t,fp_t)) env) 
                                in
                                if S.is_defined_for_args current_val args then begin
                                  match S.get_value_for_args current_val args with
                                    S.Base(ns) -> let value = S.Base ns in 
                                                  V.console_out V.Detailed v_lvl 
                                                    (fun () -> ind ^ "Case Var - Value: " ^ S.to_string value);
                                                  (value,env)
                                  | S.Fun(map) -> assert false
                                end else begin
                                  match fp_t with 
                                    F.NoFP -> assert false 
                                  | F.LFP ->  V.console_out V.Detailed v_lvl 
                                                (fun () -> ind ^ "Case Var - Value: {} (undef)");
                                              let empty_base = S.empty_base lts in
                                              (
                                                empty_base, 
                                                VarMap.set  
                                                  (Var(var,var_t,fp_t)) 
                                                  (S.set_value_for_args (empty_base) current_val args) 
                                                  env
                                              )
                                  | F.GFP ->  let value = S.full_base lts in
                                              V.console_out V.Detailed v_lvl 
                                                (fun () -> ind ^ "Case Var - Value:" ^ (S.to_string value) ^ "(undef)");
                                              (
                                                value, 
                                                VarMap.set  (Var(var,var_t,fp_t)) 
                                                  (S.set_value_for_args (value) current_val args) env
                                              )
                                end                    
    | F.Neg(phi) -> V.console_out V.Detailed v_lvl 
                      (fun () -> ind ^ "Case: Negation of " ^ F.to_string phi);
                    let (value,env) = model_check' phi lts args env v_lvl (ind ^ "  ") lazzfp in
                    (
                      match value with 
                        S.Base bdd -> let not_bdd = S.Base(MLBDD.dnot bdd) in 
                                      V.console_out V.Detailed v_lvl 
                                        (fun () -> ind ^ "Case Negation - Value: " ^ S.to_string value);
                                      (not_bdd, env)
                      | _ -> assert false
                    )

    | F.Conj(phi1, phi2) -> V.console_out V.Detailed v_lvl 
                              (fun () ->  ind ^ "Case: Conj of " ^ F.to_string phi1 
                                ^ " and " ^ F.to_string phi2
                              );
                            let (val1,env1) = model_check' phi1 lts args env v_lvl (ind ^ "  ") lazzfp in
                            let (val2,env2) = model_check' phi2 lts args env1 v_lvl (ind ^ "  ") lazzfp in
                            (
                              match val1,val2 with 
                                S.Base bdd1, S.Base bdd2 -> let and_bdd = S.Base(MLBDD.dand bdd1 bdd2) in
                                                            V.console_out V.Detailed v_lvl 
                                                              (fun () ->  ind 
                                                                ^ "Case Conj - Value: " ^ S.to_string and_bdd
                                                              
                                                              );
                                                            (and_bdd, env) 
                              | _ -> assert false
                            )

    | F.Disj(phi1, phi2) -> V.console_out V.Detailed v_lvl 
                              (fun () ->  ind ^ "Case: Disj of " 
                                ^ F.to_string phi1 ^ " and " ^ F.to_string phi2
                              );
                            let (val1,env1) = model_check' phi1 lts args env v_lvl (ind ^ "  ") lazzfp in
                            let (val2,env2) = model_check' phi2 lts args env1 v_lvl (ind ^ "  ") lazzfp in
                            (
                              match val1,val2 with 
                                S.Base bdd1, S.Base bdd2 -> let or_bdd = S.Base(MLBDD.dor bdd1 bdd2) in
                                                            V.console_out V.Detailed v_lvl 
                                                              (fun () ->  ind 
                                                                ^ "Case Disj - Value: " ^ S.to_string or_bdd
                                                              
                                                              );
                                                            (or_bdd, env) 
                              | _ -> assert false
                            )
    | F.Impl(phi1, phi2) -> V.console_out V.Detailed v_lvl 
                              (fun () ->  ind ^ "Case: Impl of " 
                                ^ F.to_string phi1 ^ " and " ^ F.to_string phi2              
                              );
                            let (val1,env1) = model_check' phi1 lts args env v_lvl (ind ^ "  ") lazzfp in
                            let (val2,env2) = model_check' phi2 lts args env1 v_lvl (ind ^ "  ") lazzfp in
                            (
                              match val1,val2 with 
                                S.Base bdd1, S.Base bdd2 -> let impl_bdd = S.Base(MLBDD.imply bdd1 bdd2) in
                                                            V.console_out V.Detailed v_lvl 
                                                              (fun () ->  ind 
                                                                ^ "Case Impl - Value: " ^ S.to_string impl_bdd
                                                              
                                                              );
                                                            (impl_bdd, env) 
                              | _ -> assert false
                            )
    | F.Equiv(phi1, phi2) ->  V.console_out V.Detailed v_lvl 
                                (fun () ->  ind ^ "Case: Equiv of " 
                                  ^ F.to_string phi1 ^ " and " ^ F.to_string phi2        
                                );
                              let (val1,env1) = model_check' phi1 lts args env v_lvl (ind ^ "  ") lazzfp in
                              let (val2,env2) = model_check' phi2 lts args env1 v_lvl (ind ^ "  ") lazzfp in
                              (
                                match val1,val2 with 
                                  S.Base bdd1, S.Base bdd2 -> let eq_bdd = S.Base(MLBDD.eq bdd1 bdd2) in
                                                              V.console_out V.Detailed v_lvl 
                                                                (fun () ->  ind 
                                                                  ^ "Case Equiv - Value: " ^ S.to_string eq_bdd
                                                                
                                                                );
                                                              (eq_bdd, env) 
                                | _ -> assert false
                              )
    | F.Diamond(trans, phi) ->  V.console_out V.Detailed v_lvl 
                                  (fun () ->  ind ^ "Case: Diamond " 
                                    ^ trans ^ " of " ^ F.to_string phi                  
                                  );
                                let (bdd,env) = model_check' phi lts args env v_lvl (ind ^ "  ") lazzfp in
                                (
                                  match bdd with
                                    S.Base bdd -> let diamond_bdd = S.Base
                                                      (Bddlts.get_diamond_trans_of lts trans bdd)
                                                  in
                                                  V.console_out V.Detailed v_lvl 
                                                    (fun () ->  ind ^ 
                                                      "Case Diamond - Value: " ^ S.to_string diamond_bdd
                                                    );
                                                  (diamond_bdd,env)
                                  | _ -> assert false
                                )
    | F.Box(trans, phi) ->  V.console_out V.Detailed v_lvl 
                              (fun () -> ind ^ "Case: Box " ^ trans ^ " of " ^ F.to_string phi);
                            let (bdd,env) = model_check' phi lts args env v_lvl (ind ^ "  ") lazzfp in
                            ( 
                              match bdd with 
                                S.Base bdd -> let box_bdd = S.Base
                                                  (Bddlts.get_box_trans_of lts trans bdd)
                                              in
                                              V.console_out V.Detailed v_lvl 
                                                (fun () -> ind ^ "Case Box - Value: " ^ S.to_string box_bdd);
                                              (box_bdd,env)
                              | _ -> assert false
                            )

  | F.Mu(var,var_t,phi) ->  V.console_out V.Detailed v_lvl 
                                (fun () -> ind ^ "Case: LFP of " ^ F.to_string phi 
                                    ^ " regarding variable " ^ var
                                );
                            let fp_var = F.Var(var,var_t,LFP) in
                              if lazzfp then begin
                              let rec repeat_until map =
                                (*repeat until approximation is found (i.e. fixpoint) 
                                  returns env including fp approximation for fp_var.
                                *)
                                let update = ref 
                                  (
                                    match var_t with 
                                      F.Base -> S.empty_base lts 
                                    | _ -> S.empty_fun
                                  ) 
                                in
                                let defined_args = 
                                      S.get_defined_arguments (VarMap.get fp_var map) 
                                in
                                List.iter
                                  (*for each argument list*) 
                                  (fun arg_list ->
                                    V.console_out V.Detailed v_lvl 
                                      (fun () -> ind ^ "  " ^ "LFP Arg: " 
                                        ^ Tools.list_to_string arg_list S.to_string
                                      );
                                    let (value, env_tmp) = (*value and (updated env) of approx for current argument*)
                                      model_check' phi lts arg_list map v_lvl  (ind ^ "     ") lazzfp
                                    in
                                    let defined_args_tmp = S.get_defined_arguments (VarMap.get fp_var env_tmp) in
                                    List.iter (*add new arguments to update, which occured first time in this iteration*) 
                                      (fun arg_list_tmp -> 
                                        update := 
                                        S.set_value_for_args 
                                          (S.get_value_for_args (VarMap.get fp_var env_tmp) arg_list_tmp) 
                                          !update arg_list_tmp
                                      ) 
                                      (List.filter (fun x -> not (List.mem x defined_args)) defined_args_tmp);
                                    update := S.set_value_for_args (value) !update arg_list
                                  ) 
                                  defined_args;
                                if S.equal !update (VarMap.get fp_var map) then begin
                                (*last and second to last iteration are equal *)
                                  V.console_out V.Detailed v_lvl 
                                    (fun () -> ind ^ "FP found.");
                                  map
                                end
                                else begin
                                (*last and second to last iteration differ *)
                                  V.console_out V.Detailed v_lvl 
                                    (fun () -> ind ^ "Approximation changed.");
                                  repeat_until (VarMap.set fp_var !update map)
                                end
                              in
                              let initialized_map = 
                                    VarMap.set 
                                      fp_var 
                                      (
                                        S.set_value_for_args 
                                          (S.empty_base lts)
                                          (
                                            match var_t with F.Base -> 
                                              S.empty_base lts 
                                            | _ -> S.empty_fun
                                          ) 
                                          args
                                      ) 
                                      env 
                              in
                              let approx = VarMap.get fp_var (repeat_until initialized_map) in
                              let value = S.get_value_for_args approx args in 
                              (
                                match value with
                                  S.Base base ->  V.console_out V.Detailed v_lvl 
                                                    (fun () -> ind ^ "FP - Value:" ^ S.to_string value);
                                                  (value,env)
                                | S.Fun map -> assert false
                              )
                            end 
                            else begin 
                              let rec get_argtypes fpformula = 
                                match fpformula with 
                                  F.Lambda (var,vart,phi) -> vart :: get_argtypes phi
                                | _ -> []
                              in
                              let rec add_to_all_args argslist newargs =
                                match newargs with 
                                  [] -> []
                                | h::t ->  (List.fold_left 
                                              (fun newarglist oldarg ->
                                                (h :: oldarg) :: newarglist
                                              )
                                              []
                                              argslist) @ add_to_all_args argslist t
                              in
                              let all_argtypes_rev = List.rev (get_argtypes phi)
                              in
                              let all_args =  if all_argtypes_rev = [] then
                                                [[]]
                                              else
                                                List.fold_left
                                                  (fun argslist argtype ->
                                                    let all_of_type = S.all_of_type lts argtype in 
                                                    add_to_all_args argslist all_of_type
                                                  )
                                                  ( List.map
                                                      (fun elem -> [elem])
                                                      (S.all_of_type lts (List.hd all_argtypes_rev))
                                                  )
                                                  (List.tl all_argtypes_rev)
                              in
                              let rec repeat_until map =
                                let update = ref 
                                  (
                                    match var_t with 
                                      F.Base -> S.empty_base lts 
                                    | _ -> S.empty_fun
                                  ) 
                                in
                                List.iter
                                  (fun arg_list ->
                                    let (value, env_tmp) = (*value and (updated env) of approx for current argument*) 
                                      model_check' phi lts arg_list map v_lvl (ind ^ "     ") lazzfp
                                    in
                                    update := S.set_value_for_args value !update arg_list 
                                  ) 
                                  all_args;
                                if S.equal !update (VarMap.get fp_var map) then
                                  map
                                else
                                  repeat_until (VarMap.set fp_var !update map)
                              in
                              let initialized_map = 
                                    VarMap.set 
                                      fp_var 
                                      (
                                        match var_t with 
                                          F.Base -> S.empty_base lts 
                                        | _ -> S.empty_fun
                                      ) 
                                      env 
                              in
                              let approx = VarMap.get fp_var (repeat_until initialized_map) in
                              (
                                match S.get_value_for_args approx args with
                                  S.Base base -> (S.Base base,env)
                                | S.Fun map -> assert false
                              )
                            end

  | F.Nu(var,var_t,phi) ->  V.console_out V.Detailed v_lvl 
                                (fun () -> ind ^ "Case: GFP of " ^ F.to_string phi);
                            let fp_var = F.Var(var,var_t,GFP) in
                            if lazzfp then begin
                              let rec repeat_until map =
                                (*repeat until approximation is found (i.e. fixpoint) 
                                  returns env including fp approximation for fp_var.
                                *)
                                let update = ref 
                                  (
                                    match var_t with 
                                      F.Base -> S.full_base lts 
                                    | _ -> S.empty_fun
                                  ) 
                                in
                                let defined_args = 
                                      (S.get_defined_arguments (VarMap.get fp_var map)) 
                                in
                                List.iter
                                  (*for each argument list*) 
                                  (fun arg_list ->
                                    V.console_out V.Detailed v_lvl 
                                      (fun () -> ind ^ "  " ^ "GFP Arg: " 
                                        ^ Tools.list_to_string arg_list S.to_string
                                      );
                                    let (value, env_tmp) = (*value and (updated env) of approx for current argument*) 
                                      model_check' phi lts arg_list map v_lvl (ind ^ "     ") lazzfp
                                    in
                                    let defined_args_tmp = S.get_defined_arguments (VarMap.get fp_var env_tmp) in
                                      List.iter (*add new arguments to update, which occured first time in this iteration*) 
                                      (fun arg_list_tmp -> 
                                        update := 
                                        S.set_value_for_args 
                                          (S.get_value_for_args (VarMap.get fp_var env_tmp) arg_list_tmp) 
                                          !update arg_list_tmp
                                      ) 
                                    (List.filter (fun x -> not (List.mem x defined_args)) defined_args_tmp);
                                    update := S.set_value_for_args (value) !update arg_list
                                  ) 
                                  defined_args;
                                if S.equal !update (VarMap.get fp_var map) then begin
                                  (*last and second to last iteration are equal *)
                                  V.console_out V.Detailed v_lvl (fun () -> ind ^ "FP found.");
                                  map
                                end
                                else begin
                                  (*last and second to last iteration differ *)
                                  V.console_out V.Detailed v_lvl (fun () -> ind ^ "Approximation changed.");
                                  repeat_until (VarMap.set fp_var !update map)
                                end
                              in
                              let initialized_map = 
                                    VarMap.set 
                                      fp_var 
                                      (
                                        S.set_value_for_args 
                                          (S.full_base lts)
                                          (
                                            match var_t with 
                                              F.Base -> S.full_base lts 
                                            | _ -> S.empty_fun
                                          ) 
                                          args
                                      ) 
                                      env 
                              in
                              let approx = VarMap.get fp_var (repeat_until initialized_map) in
                              let value = S.get_value_for_args approx args in
                              (
                                match value with
                                  S.Base base ->  V.console_out V.Detailed v_lvl 
                                                    (fun () -> ind ^ "FP - Value:" ^ S.to_string value);
                                                  (value,env)
                                | S.Fun map -> assert false
                              )
                              end
                            else begin 
                              let rec get_argtypes fpformula = 
                                match fpformula with 
                                  F.Lambda (var,vart,phi) -> vart :: get_argtypes phi
                                | _ -> []
                              in
                              let rec add_to_all_args argslist newargs =
                                match newargs with 
                                  [] -> []
                                | h::t ->  (List.fold_left 
                                              (fun newarglist oldarg ->
                                                (h :: oldarg) :: newarglist
                                              )
                                              []
                                              argslist
                                            ) @ add_to_all_args argslist t
                              in
                              let all_argtypes_rev = List.rev (get_argtypes phi)
                              in
                              let all_args =  if all_argtypes_rev = [] then
                                                [[]]
                                              else
                                                List.fold_left
                                                  (fun argslist argtype ->
                                                    let all_of_type = S.all_of_type lts argtype in 
                                                    add_to_all_args argslist all_of_type
                                                  )
                                                  ( List.map
                                                      (fun elem -> [elem])
                                                      (S.all_of_type lts (List.hd all_argtypes_rev))
                                                  )
                                                  (List.tl all_argtypes_rev)
                              in
                              let rec repeat_until map =
                                let update = ref 
                                  (
                                    match var_t with 
                                      F.Base -> S.full_base lts 
                                    | _ -> S.empty_fun
                                  ) 
                                in
                                List.iter
                                  (fun arg_list ->
                                    let (value, env_tmp) = (*value and (updated env) of approx for current argument*) 
                                      model_check' phi lts arg_list map v_lvl  (ind ^ "     ") lazzfp
                                    in
                                    update := S.set_value_for_args value !update arg_list 
                                  ) 
                                  all_args;
                                if S.equal !update (VarMap.get fp_var map) then
                                  map
                                else
                                  repeat_until (VarMap.set fp_var !update map)
                              in
                              let initialized_map = 
                                    VarMap.set 
                                      fp_var 
                                      (
                                        match var_t with 
                                          F.Base -> S.full_base lts 
                                        | _ -> S.empty_fun
                                      ) 
                                      env 
                              in
                              let approx = VarMap.get fp_var (repeat_until initialized_map) in
                              (
                                match S.get_value_for_args approx args with
                                  S.Base base -> (S.Base base,env)
                                | S.Fun map -> assert false
                              )
                            end

  | F.Lambda(var,var_t,phi) ->  V.console_out V.Detailed v_lvl 
                                  (fun () ->  ind ^ "Case: Lambda " ^ var ^ " of " 
                                              ^ F.to_string phi
                                  );
                                let (value,env) = 
                                  model_check' 
                                    phi lts 
                                    (List.tl args) 
                                    (VarMap.set (F.Var(var,var_t,F.NoFP)) (List.hd args) env) 
                                    v_lvl  (ind ^ "  ") lazzfp
                                in
                                V.console_out V.Detailed v_lvl 
                                  (fun () ->  ind ^ "Case Lambda " 
                                    ^ var ^ " - Value: " ^ S.to_string value
                                  );
                                (value,env)

  | F.App(phi1, phi2) ->    V.console_out V.Detailed v_lvl 
                              (fun () ->  ind ^ "Case: App of " ^ F.to_string phi2 ^ " to " 
                                          ^ F.to_string phi1
                              );
                            let phi_2_sem = fully_calc phi2 lts [] env v_lvl ind in 
                            let (value, env) = model_check' phi1 lts (phi_2_sem :: args) env v_lvl  (ind ^ "  ") lazzfp in
                            V.console_out V.Detailed v_lvl 
                              (fun () ->  ind ^ "Case App - Value: " 
                                          ^ S.to_string value
                              );
                            (value,env)
                            
  and fully_calc formula lts args env v_lvl indent =
    let rec helper formula lts args env v_lvl = 
      match formula with
        F.Lambda(var, var_t, phi)  ->   let all_args = S.all_of_type lts var_t in
                                        List.fold_left
                                          (fun sem arg ->  
                                                          S.set_value_for_args
                                                            (helper phi lts (args) 
                                                              (VarMap.set (F.Var(var,var_t,F.NoFP)) arg env) v_lvl
                                                            ) 
                                                            sem 
                                                            [arg]
                                          ) 
                                          (S.empty_fun) all_args 
                                      
      | F.Mu(var, var_t, phi) ->  let (value,_) = model_check' 
                                                    (F.Mu(var, var_t, phi)) 
                                                    lts (args) 
                                                    env V.None  "" true (*TODO: include lazyness*)
                                  in value
                                  (*TODO: only fp arguments of type * -> base are possible with this *)
                                  (*TODO: Wait what if \lambda is below fp. does this work? *)
                                  (*TODO: GFP!! needed*)
                                    
      | F.Var(var,var_t,fp_t) ->  if VarMap.mem (F.Var(var,var_t,fp_t)) env then
                                    S.get_value_for_args (VarMap.get (F.Var(var,var_t,fp_t)) env) args
                                  else assert false

      | F.App(phi_1,phi_2) -> let (value,_) = 
                                model_check' phi_1 lts ((helper phi_2 lts [] env v_lvl) :: args) env V.None  "" true
                              in value

      | _ -> let (value,_) = model_check' formula lts [] env V.None "" true in value
    in
    V.console_out V.Detailed v_lvl (fun () -> indent ^ "Calculate: " ^ F.to_string formula ^ " completely...");
    let value = helper formula lts args env v_lvl in
    value
  in
  let (bdd,_) = 
        model_check' formula lts arguments environment v_lvl indent lazzyfp in
  match bdd with 
    S.Base bdd -> Bddlts.get_all_sats lts bdd 
  | _ -> assert false

let model_check ?(v_lvl=V.Info) ?(lazzyfp=true) formula lts =
  V.console_out V.Info v_lvl (fun () -> "MODEL CHECKER INPUT:");
  V.console_out V.Info v_lvl (fun () -> Bddlts.to_string lts ^"\n");
  V.console_out V.Info v_lvl (fun () -> "FORMULA: " ^ F.to_string ~show_types:false formula ^ "\n\n");
  V.console_out V.Info v_lvl (fun () -> "Start model checker...");
  let start_time = Unix.gettimeofday () in
  let value = model_check formula lts [] VarMap.empty v_lvl "  " lazzyfp in
  let end_time = Unix.gettimeofday () in 
  V.console_out V.Info v_lvl (fun () -> "RESULT: " ^ Tools.list_to_string value string_of_int);
  V.duration_out V.Info v_lvl start_time end_time "";
  value