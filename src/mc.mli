open Hfl

val model_check : ?v_lvl:Verbose.lvl -> Formula.t -> Bddlts.t  -> Bddlts.state list 