open Hfl
open Datastructures
open Lts
open Mc

let _ =
  let open Formula in
  let f' = App(Mu("X", Fun(Base,Base), Lambda("x",Base, Disj(Var("x",Base,NoFP), App(Var("X",Fun(Base,Base),LFP),Diamond("a", Var("x",Base,NoFP)))))), Prop("p")) in
  let f = Lambda("Y", Fun(Base,Base), App(Var("Y", Fun(Base,Base),NoFP), Prop("p"))) in
  let f'' = Lambda("Z", (Fun(Base,Base)), Var("Z", Fun(Base,Base),NoFP)) in
  let lts = Lts.create_random 10 ["a"] ["p"] 0.1 0.2 in
  (*print_endline (Formula.to_string f')*)
  model_check ~v_lvl:Verbose.Detailed f' lts;
  (*let map_one = ref (fully_calc_sem ~v_lvl:Verbose.Debug f'' lts) in 
  let map_two = map_one in
  ()*)
  (*map_one := Semantics.set_value_for_args (Semantics.Base(NodeSet.of_node_list [0;2])) !map_one [(Semantics.Base(NodeSet.of_node_list [0;2]))];
  map_two := Semantics.Base NodeSet.empty;
  print_endline (Formula.to_string f);
  print_endline (Lts.to_string lts);
  print_endline (Semantics.to_string !map_one ^ "\n");
  print_endline (string_of_int (Semantics.compare !map_two !map_one))*)