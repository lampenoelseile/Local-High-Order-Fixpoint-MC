open Datastructures
open Lts
open Hfl
open Verbose
open Tcsset

val model_check : ?verb_lvl:Verbose.lvl -> Lts.t -> Formula.t -> NodeSet.t