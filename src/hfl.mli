open Datastructures
open Tcsset


module Semantics : sig
  (*interface: Semantics module
    Represents the semantics of a hfl formula interpreted over some lts
  *)
  type t =
    (* type of semantics
      base type is a set of nodes (Base)
      higher order types are represented in form of a map (Fun)
    *)
    | Base of NodeSet.t
    | Fun of (t,t) TreeMap.t

  val get_value_for_args : t -> t list -> t
  (** Returns value of semantic object for given arguments. 
      @param t semantic object 
      @param t list arguments
      @return value for given arguments
  *)
  val set_value_for_args : t -> t -> t list -> t
  (** Sets value of semantic object for given arguments. 
      @param t value to be set
      @param t semantic object 
      @param t list arguments
      @return value for given arguments
  *)
  val is_defined_for_args : t -> t list -> bool
  (** Checks wether value of a semantic object is defined for given arguments.
      @param t semantic object 
      @param t list arguments
      @return if value is defined
  *)
end

module Formula : sig
  type variable_t =
    (* type of variables. 
       Note: same names as Semantics.t
    *)
    | Base 
    | Fun of variable_t * variable_t

  type t =
    (* type of formulas. *)
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

  val string_of_formula : ?show_types:bool -> t -> string
  (** Converts formula into string representation.

      @param bool flag if types should be printed
      @param formula formula to be converted
      @return string  string representation of formula
  *)
end