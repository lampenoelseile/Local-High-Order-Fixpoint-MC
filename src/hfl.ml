(* For high-level info about public parts see formulas.mli *)
open Datastructures
open Tcsset

(* TODO: check whats needed and comment remaining.*)

module Semantics = struct
  type t =
    | Base of NodeSet.t
    | Fun of (t,t) TreeMap.t
  
  let rec get_value_for_args = function
    | Base(ns) -> (function
                  | [] -> Base(ns)
                  | _ -> print_endline "Error."; Base(ns))  
    | Fun(map) -> (function
                  | [] -> print_endline "Error."; Fun(map)
                  | h :: t -> get_value_for_args (TreeMap.find h map) t)
  
  let rec set_value_for_args value = function
    | Base(ns) -> (function
                    | [] -> value
                    | _ -> print_endline "Error: Arguments - Sem missmatch."; Base(ns))
    | Fun(map) ->  (function
                    | [] -> print_endline "Error: Arguments - Sem missmatch."; Fun(map)
                    | h :: t -> Fun(TreeMap.add h (set_value_for_args value (TreeMap.find h map) t) map))

  let rec is_defined_for_args = function
    | Base(ns) -> (function
                  | [] -> true
                  | _ -> print_endline "Error."; false)
    | Fun(map) -> (function
                  | [] -> print_endline "Error."; false
                  | h :: t -> if TreeMap.mem h map then true else is_defined_for_args (TreeMap.find h map) t)
end

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

  let string_of_formula ?(show_types=false) phi = 
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