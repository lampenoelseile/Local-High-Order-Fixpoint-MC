type v_type = Base | Fun of v_type * v_type

type formula =
    Const of bool
  | Prop of string
  | Var of string * v_type
  | Int of int
  | Neg of formula
  | Conj of formula * formula
  | Disj of formula * formula
  | Impl of formula * formula
  | Equiv of formula * formula
  | Diamond of string * formula
  | Box of string * formula
  | Mu of string * v_type * formula
  | Nu of string * v_type * formula
  | Lambda of string * v_type * formula
  | App of formula * formula

val string_of_v_type : v_type -> string

val string_of_formula : ?show_types:bool -> formula -> string
(** Converts formula into string representation.

    @param bool flag if types should be printed
    @param formula formula to be converted
    @return string  string representation of formula
*)