open Hfl

val model_check : ?v_lvl:Verbose.lvl -> Formula.t -> Bddlts.t  -> Bddlts.state list
(** Lazy model checker for HFL. Works on bdds and achieves efficiency due to 
    partial computation of non-argument FPs. 
    @param Verbose.lvl determines output level
    @param Formula.t hfl formula
    @param Bddlts.t lts represented via bdds 
    @return list of satisfying states
*)