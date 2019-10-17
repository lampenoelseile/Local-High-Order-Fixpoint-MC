open Basedata
open Tcsset

module Formula : sig
  type variable_t =
    (* type of variables. 
       Note: same names as Semantics.t
    *)
    | Base 
    | Fun of variable_t * variable_t

  type fp_t =
    NoFP
  | LFP
  | GFP

  type t =
    (* type of formulas. *)
    | Const of bool
    | Prop of string
    | Var of string * variable_t * fp_t
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

  val to_string : ?max_length:int -> ?show_types:bool -> t -> string
  (** Converts formula into string representation.

      @param int maximum length of string
      @param bool flag if types should be printed
      @param formula formula to be converted
      @return string  string representation of formula
  *)
end

module Semantics : sig
  type t =
    Base of MLBDD.t
  | Fun of (t,t) TreeMap.t

  val compare : t -> t -> int

  val empty_base : Bddlts.t -> t
  val full_base : Bddlts.t -> t  
  val empty_fun : t

  val to_string : ?max_length:int -> t -> string

  val is_defined_for_args : t -> t list -> bool

  val get_value_for_args : t -> t list -> t
  
  val set_value_for_args : t -> t -> t list -> t

  val get_defined_arguments : t -> (t list) list

  val all_of_type : Bddlts.t -> Formula.variable_t -> t list
end