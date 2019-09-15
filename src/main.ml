open Hfl
open Datastructures
open Lts
open Mc

let _ =
  let open Formula in
  let f' = App(Lambda("X", Fun(Base, Base), Equiv( App(Var("X", Fun(Base, Base)), Prop("p")), Const(false))), Lambda("Y", Base, Var("Y", Base))) in
  let f = Lambda("Y", Fun(Base,Base), App(Var("Y", Fun(Base,Base)), Prop("p"))) in
  let lts = Lts.create_random 1 ["a"; "b"; "c"] ["p"; "q"; "r"; "s"] 0.5 0.8 in
  (*model_check ~verb_lvl:Verbose.Debug lts f';*)
  let map_one = ref (fully_calc_sem ~verb_lvl:Verbose.Detailed lts f) in 
  let map_two = map_one in
  (*map_one := Semantics.set_value_for_args (Semantics.Base(NodeSet.of_node_list [0;2])) !map_one [(Semantics.Base(NodeSet.of_node_list [0;2]))];
  map_two := Semantics.Base NodeSet.empty;*)
  print_endline (Formula.to_string f);
  print_endline (Lts.to_string lts);
  print_endline (Semantics.to_string !map_one ^ "\n");
  print_endline (string_of_int (Semantics.compare !map_two !map_one))