open Datastructures
open Lts
open Hfl
open Verbose
open Tcsset

val model_check : ?v_lvl:Verbose.lvl -> Formula.t -> Lts.t  -> NodeSet.t

val fully_calc_sem : ?v_lvl:Verbose.lvl -> Formula.t -> Lts.t -> Semantics.t