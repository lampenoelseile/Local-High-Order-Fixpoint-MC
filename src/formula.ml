(* TODO: check whats needed and comment remaining.*)
type v_type = 
  | Base
	| Fun of v_type * v_type

type t = 
  | Const of bool
  | Prop of string
  | Var of string * v_type
  | Neg of t
  | Conj of t * t
  | Disj of t * t
  | Impl of t * t
  | Equiv of t * t
  | Diamond of string * t
  | Box of string * t
  | Mu of string * v_type * t
  | Nu of string * v_type * t 
  | Lambda of string * v_type * t
  | App of t * t


let rec string_of_v_type = function
  | Base -> "base"
  | Fun(a,b) -> Format.sprintf "%s -> %s" (string_of_v_type a) (string_of_v_type b)

let string_of_formula ?(show_types=false) phi = 
  let s_type id t = 
    if show_types then Format.sprintf "(%s:%s)" id (string_of_v_type t)
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