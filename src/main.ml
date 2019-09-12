open Hfl
open Datastructures
open Lts
open Mc

let _ =
  let open Formula in
  let f' = App(Lambda("X", Fun(Base, Base), Equiv( App(Var("X", Fun(Base, Base)), Prop("p")), Const(false))), Lambda("Y", Base, Var("Y", Base))) in
  let f = Conj(Diamond("c",Prop("r")), Neg(Prop("r")))in
  let f'' = Box("a",Prop("r")) in
  let lts = Lts.create_random 5 ["a"; "b"; "c"] ["p"; "q"; "r"; "s"] 0.5 0.8 in
  model_check lts f';
  model_check lts f'';