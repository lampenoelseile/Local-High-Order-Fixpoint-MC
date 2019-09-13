(* For high-level info about public parts see formulas.mli *)
open Datastructures
open Tcsset
module V = Verbose

(* TODO: check whats needed and comment remaining.*)
module Formula = struct
  type variable_t = 
  | Base
  | Fun of variable_t * variable_t

  let rec string_of_var_t = function
  | Base -> "base"
  | Fun(a,b) -> Format.sprintf "%s -> %s" (string_of_var_t a) (string_of_var_t b)
  type t = 
    | Const of bool
    | Prop of string
    | Var of string * variable_t
    | Neg of t
    | Conj of t * t
    | Disj of t * t
    | Impl of t * t
    | Equiv of t * t
    | Diamond of string * t
    | Box of string * t
    | Mu of string * variable_t * t
    | Nu of string * variable_t * t 
    | Lambda of string * variable_t * t
    | App of t * t

  let to_string ?(show_types=false) phi = 
    let s_type id t = 
      if show_types then Format.sprintf "(%s:%s)" id (string_of_var_t t)
      else id in
    let rec f = function
      | Const(b) -> if b then "true" else "false"
      | Prop(prop_var) -> Format.sprintf "%s" prop_var
      | Var(var,t) -> s_type var t
      | Neg(phi) -> Format.sprintf "~%s" (f phi) 
      | Conj(phi1, phi2) -> Format.sprintf "(%s && %s)" (f phi1) (f phi2) 
      | Disj(phi1, phi2) -> Format.sprintf "(%s || %s)" (f phi1) (f phi2) 
      | Impl(phi1, phi2) -> Format.sprintf "(%s => %s)" (f phi1) (f phi2) 
      | Equiv(phi1, phi2) -> Format.sprintf "(%s <=> %s)" (f phi1) (f phi2) 
      | Diamond(action, phi) -> Format.sprintf "<%s> %s" action (f phi) 
      | Box(action, phi) -> Format.sprintf "[%s] %s" action (f phi) 
      | Mu(var,t,phi) -> Format.sprintf "(mu %s. %s)" (s_type var t) (f phi) 
      | Nu(var,t,phi) -> Format.sprintf "(nu %s. %s)" (s_type var t) (f phi) 
      | Lambda(var,t,phi) -> Format.sprintf "(lam %s %s)" (s_type var t) (f_lambda phi)
      | App(phi,psi) -> Format.sprintf "(%s %s)" (f_app phi) (f psi)
    and f_lambda = function
      | Lambda(var,t,phi) ->
    Format.sprintf "%s %s" (s_type var t) (f_lambda phi)
      | _ as phi -> 
    Format.sprintf "-> %s" (f phi)
    and f_app = function
      | App(phi,psi) ->
    Format.sprintf "%s %s" (f_app phi) (f psi)
      | _ as phi -> f phi
    in f phi
end
  
module Semantics = struct
  type t =
    | Base of NodeSet.t
    | Fun of (t,t) TreeMap.t
  
  let empty_base = Base(NodeSet.empty)
  let empty_fun = Fun(TreeMap.empty compare)

  let rec to_string sem = 
    match sem with
    | Base(ns) -> NodeSet.to_string ns
    | Fun(map) -> (TreeMap.fold (fun key value str -> str ^ "[" ^ to_string key ^ "->" ^ to_string value ^ "],") map "[") ^ "]"

  (*TODO: Make output compact, add module etc*)
  let rec is_defined_for_args ?(v_lvl=V.None) = function
    | Base(ns) -> (function
                  | [] -> V.console_out V.Debug v_lvl (fun () -> "Object was of type Base and arglist empty: " ^ to_string (Base ns)); true
                  | _ ->  V.console_out V.Debug v_lvl (fun () -> "Object was of type Base but arglist nonempty - Obj:" ^ to_string (Base ns)); 
                          assert false)
    | Fun(map) -> (function
                  | [] -> V.console_out V.Debug v_lvl (fun () -> "Object was not of type Base but arglist empty: " ^ to_string (Fun map));  
                          assert false
                  | h :: t ->  let value = TreeMap.mem h map in
                                V.console_out V.Debug v_lvl (fun () -> "Definition of sem for " ^ to_string (Fun map) 
                                                                        ^ " for "^ to_string h ^ " is "^ Bool.to_string value);
                                value && is_defined_for_args ~v_lvl (TreeMap.find h map) t)

  let rec get_value_for_args = function
    | Base(ns) -> (function
                  | [] -> Base(ns)
                  | _ -> print_endline "Error."; Base(ns))  
    | Fun(map) -> (function
                  | [] -> print_endline "Error."; Fun(map)
                  | h :: t -> get_value_for_args (TreeMap.find h map) t)
  
  (*TODO: Make verbose compact, add module etc*)
  let rec set_value_for_args ?(v_lvl=V.None) value = function
    | Base(ns) -> (function
                    | [] -> V.console_out V.Debug v_lvl (fun () -> "Object was of type Base and arglist empty: " ^ to_string (Base ns));
                            value
                    | _ ->  V.console_out V.Debug v_lvl (fun () -> "Object was of type Base but arglist nonempty: " ^ to_string (Base ns)); 
                            assert false)
    | Fun(map) ->  (function
                    | [] -> V.console_out V.Debug v_lvl (fun () -> "Object was not of type Base but arglist empty: " ^ to_string (Fun map)); 
                            assert false
                    | h :: [] -> V.console_out V.Debug v_lvl (fun () -> "Argument " ^ to_string h ^ " is set for arg -> ns object.");
                                  Fun(TreeMap.add h value map);
                    | h :: t -> V.console_out V.Debug v_lvl (fun () -> "Argument " ^ to_string h ^ " is set for fun object.");
                                if is_defined_for_args ~v_lvl (Fun map) [h] then
                                  Fun(TreeMap.add h (set_value_for_args ~v_lvl value (TreeMap.find h map) t) map)
                                else
                                  let new_map = empty_fun in 
                                  Fun(TreeMap.add h (set_value_for_args ~v_lvl value new_map t) map))
end

(*SET OF SEMANTICS *)
module SemanticsSet = struct
  type t = Semantics.t TreeSet.t

  let empty = TreeSet.empty compare
  let add = TreeSet.add
  let iter = TreeSet.iter
  let fold = TreeSet.fold

  let rec all_of_type lts = function
      Formula.Base -> NodeSet.fold_subsets (fun ns sems -> add (Semantics.Base ns) sems) (Lts.get_all_nodes lts) empty
    | Formula.Fun(key_t, val_t) -> empty

  let to_string sems =
    (fold (fun sem str -> match sem with
                            Semantics.Base(ns) -> str ^ NodeSet.to_string ns ^ ","
                          | Semantics.Fun(map) -> str ^ "HO" ^ "," ) 
          sems "[") ^ "]"
end