open Datastructures
open Lts
open Formula

let rec model_check lts formula =
  match formula with
  | Const(b) -> if b then Lts.get_all_nodes lts else NodeSet.empty
  | Prop(prop) -> Lts.get_nodes_of_proposition lts prop
  | Var(var,t) -> NodeSet.empty
  | Neg(phi) -> NodeSet.diff (Lts.get_all_nodes lts) (model_check lts phi)
  | Conj(phi_1, phi_2) -> NodeSet.inter (model_check lts phi_1) (model_check lts phi_2)
  | Disj(phi_1, phi_2) -> NodeSet.union (model_check lts phi_1) (model_check lts phi_2)
  | Impl(phi_1, phi_2) -> NodeSet.union (model_check lts (Neg(phi_1))) (model_check lts phi_2)
  | Equiv(phi_1, phi_2) -> NodeSet.inter (model_check lts (Impl(phi_1,phi_2))) (model_check lts (Impl(phi_2,phi_1)))
  | Diamond(transition, phi) -> let val_diamond = ref NodeSet.empty in
                                let val_phi = model_check lts phi in 
                                NodeSet.iter (fun n -> 
                                                val_diamond := NodeSet.union !val_diamond (NodeSet.inter val_phi 
                                                                                            (Lts.get_trans_predecessors_of_node lts transition n))) 
                                                val_phi;
                                !val_diamond 
  | Box(transition, phi) -> model_check lts (Neg(Diamond(transition, Neg(phi))))
  | Mu(var,t,phi) -> NodeSet.empty 
  | Nu(var,t,phi) -> NodeSet.empty
  | Lambda(var,t,phi) -> NodeSet.empty
  | App(phi,psi) -> NodeSet.empty