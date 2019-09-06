type v_type = Base | Fun of v_type * v_type

type t =
    Const of bool
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

val string_of_v_type : v_type -> string

val string_of_formula : ?show_types:bool -> t -> string
(** Converts formula into string representation.

    @param bool flag if types should be printed
    @param formula formula to be converted
    @return string  string representation of formula
*)