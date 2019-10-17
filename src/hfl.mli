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
  (** interface: Semantics module
      Represents the semantics of a hfl formula interpreted over some lts (Bddlts)
  *)
  type t =
    (** type of semantics
        base type is a set of nodes represented as a bdd 
        higher order types are represented in form of a map
    *)
    Base of MLBDD.t
  | Fun of (t,t) TreeMap.t

  val equal : t -> t -> bool
  (** Checks wether two semantic objects are equal
      @param t sem one
      @param t sem two
      @return equal?
  *)

  val empty_base : Bddlts.t -> t
  (** Returns the empty base object / false bdd
      @param bddlts lts over which semantics are interpreted
      @return empty base object
  *)

  val full_base : Bddlts.t -> t
  (** Returns the full base object / true bdd
      @param bddlts lts over which semantics are interpreted
      @return full base object
  *)  

  val empty_fun : t
  (** Returns empty fun object
      @return uninitialized map / empty fun
  *)

  val to_string : ?max_length:int -> t -> string
  (** Returns string representation of given semantic object.
      @param t semantic object
      @return string representation
  *)

  val is_defined_for_args : t -> t list -> bool
  (** Checks wether value of a semantic object is defined for given arguments.
      @param t semantic object 
      @param t list arguments
      @return if value is defined
  *)

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

  val get_defined_arguments : t -> (t list) list
  (** Returns all arguments/argumentlists, that a (possibly partial) sem object 
      is defined for. Empty for basetype.
      @param t semantic object 
      @return valid arguments
  *)
  val all_of_type : Bddlts.t -> Formula.variable_t -> t list
  (** Returns a list of all semantic object over given lts which
      correspond to a type a hfl type.
      @param bddlts lts to get sem objs for
      @param variable_t hfl type (see Hfl.mli)
      @return all sem objects
   *)
end