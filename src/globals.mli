(* TODO: see .ml*)
(** {1 Misc **)
val list_powerset : 'a list -> 'a list list
val arith_seq: int->int->int->int list   (** (arith_seq n a b = [b,a+b,2a+b,..,(n-1)a+b]) **)
val mapi:(int->'a->'b)->'a list->'b list (** List.mapi as in version 4.00 of Ocaml) **)

val special_labels:unit->((string*string)*string) list
val load_options:string->unit
val list_setunion: 'a list -> 'a list -> 'a list
val list_setminus: 'a list -> 'a list -> 'a list
val list_cartesian_product: 'a list -> 'b list -> ('a * 'b) list
val list_nth_product: int -> 'a list -> 'a array list

(** {1 Identifiers management function} **)
module Ids : sig
  type action
  val action_of_string:string->action
  val string_of_action:action->string
  val action_of_int:int->action
  val int_of_action:action->int
  val nb_actions:unit->int
  val all_actions:unit->action list
  val dummy_action:unit->action

  type prop_var
  val prop_var_of_string:string->prop_var
  val string_of_prop_var:prop_var->string
  val prop_var_of_int:int->prop_var
  val int_of_prop_var:prop_var->int
  val nb_prop_vars:unit->int
  val all_prop_vars:unit->prop_var list

  type state
  val state_of_string:string->state
  val string_of_state:state->string
  val state_of_int:int->state
  val int_of_state:state->int
  val nb_states:unit->int
end
  
(** {1 Locations } **)
module Location : sig
  type t
  val lexbuf : Lexing.lexbuf option ref
  val mkloc : Lexing.position -> Lexing.position -> t
  val symbol_loc : unit -> t
  val none : t
  val sprint : t -> string
end


(** {1 Errors} **)
module Error : sig
  exception Illegal_character of char * Location.t
  exception Unterminated_comment of Location.t
  exception Parse_error of Location.t
  exception Unknown_identifier of string
  exception InitInvalidRank of int * int 
  exception InitInvalidState of Ids.state
  exception Type_error of string
  val report : (unit->'a) -> 'a
end
