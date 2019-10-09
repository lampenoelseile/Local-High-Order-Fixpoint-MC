open Mc
open Hfl
open Lts
module V = Verbose
open Datastructures

let build_lts ~zero_max_count ~one_max_count =
  let open Node in 
  let rec build list z_count o_count =
    match (z_count, o_count) with
      (0,0) ->  (NamedNode "w0-0") :: list
    | (z,0) ->  build ((NamedNode ("w" ^ (string_of_int z) ^ "-0")) :: list) (z-1) 0
    | (0,o) -> build ((NamedNode ("w" ^ "0-" ^(string_of_int o))) :: list) 0 (o-1)
    | (z,o) -> build ((NamedNode ("w" ^ (string_of_int z) ^ "-" ^(string_of_int o))) :: list) (z-1) (o-1) @ 
              build ((NamedNode ("w" ^ (string_of_int z) ^ "-" ^(string_of_int o))) :: list) (z) (o-1) @
              build ((NamedNode ("w" ^ (string_of_int z) ^ "-" ^(string_of_int o))) :: list) (z-1) (o)
  in
  let lts_init = List.fold_left (fun lts node -> Lts.add_node lts node) Lts.create_empty (build [] zero_max_count one_max_count) in
  let lts_write = NodeSet.fold 
    (fun lts node -> 
      let node_z_count_with_i = List.nth (String.split_on_char '-' (to_string node)) 0 in
      let node_o_count = List.nth (String.split_on_char '-' (to_string node)) 1 in
      let lts_new_z_trans = 
        NodeSet.fold 
          (fun lts2 node2 ->
            Lts.add_transition lts2 "0" node node2
          ) 
          lts 
          (NodeSet.filter 
            (fun node2 ->
              let node2_z_count_with_i = List.nth (String.split_on_char '-' (to_string node2)) 0 in
              let node2_o_count = List.nth (String.split_on_char '-' (to_string node2)) 1 in
              ((int_of_string (String.sub node_z_count_with_i 1 (String.length node_z_count_with_i-1)))+1) == 
                  (int_of_string (String.sub node2_z_count_with_i 1 (String.length node2_z_count_with_i-1))) &&
                  (int_of_string node_o_count) == (int_of_string node2_o_count)
              
            ) 
            (Lts.get_all_nodes lts)
          )
      in
      NodeSet.fold 
          (fun lts2 node2 ->
            Lts.add_transition lts2 "1" node node2
          ) 
          lts_new_z_trans
          (NodeSet.filter 
            (fun node2 ->
              let node2_z_count_with_i = List.nth (String.split_on_char '-' (to_string node2)) 0 in
              let node2_o_count = List.nth (String.split_on_char '-' (to_string node2)) 1 in
              ((int_of_string (String.sub node_z_count_with_i 1 (String.length node_z_count_with_i-1)))) == 
                  (int_of_string (String.sub node2_z_count_with_i 1 (String.length node2_z_count_with_i-1))) &&
                  ((int_of_string node_o_count)+1) == (int_of_string node2_o_count)
            ) 
            (Lts.get_all_nodes lts_new_z_trans)
          )
    ) 
    lts_init (Lts.get_all_nodes lts_init)
    in 
    let rec build2 list z_count o_count =
      match (z_count, o_count) with
      (0,0) ->  (NamedNode "r0-0") :: list
    | (z,0) ->  build2 ((NamedNode ("r" ^ (string_of_int z) ^ "-0")) :: list) (z-1) 0
    | (0,o) -> build2 ((NamedNode ("r" ^ "0-" ^(string_of_int o))) :: list) 0 (o-1)
    | (z,o) -> build2 ((NamedNode ("r" ^ (string_of_int z) ^ "-" ^(string_of_int o))) :: list) (z-1) (o-1) @ 
              build2 ((NamedNode ("r" ^ (string_of_int z) ^ "-" ^(string_of_int o))) :: list) (z) (o-1) @
              build2 ((NamedNode ("r" ^ (string_of_int z) ^ "-" ^(string_of_int o))) :: list) (z-1) (o)
  in
  let lts_read_write = 
    Lts.add_proposition 
      (List.fold_left (fun lts node -> Lts.add_node lts node) lts_write (build2 [] zero_max_count one_max_count)) 
      (NamedNode "r0-0") 
      "END"
  in
  let lts_read_write_trans =
    NodeSet.fold 
    (fun lts node -> 
      let node_z_count_with_r = List.nth (String.split_on_char '-' (to_string node)) 0 in
      let node_z_count = String.sub node_z_count_with_r 1 ((String.length node_z_count_with_r)-1) in
      let node_o_count = List.nth (String.split_on_char '-' (to_string node)) 1 in
      if (int_of_string node_z_count) > 0 then
        Lts.add_transition lts "0" node (NamedNode ("r" ^ string_of_int ((int_of_string node_z_count) -1) ^ "-" ^ node_o_count))
      else
        if (int_of_string node_o_count) > 0 then
          Lts.add_transition lts "1" node (NamedNode ("r" ^ node_z_count ^ "-" ^ string_of_int ((int_of_string node_o_count) -1)))
          else lts
    ) 
    lts_read_write 
    (
      NodeSet.filter 
        (
          fun node -> (String.get (to_string node) 0) == 'r'
        )
        (Lts.get_all_nodes lts_read_write)
    )
  in
  NodeSet.fold
    (fun lts node ->
      NodeSet.fold
        (fun lts2 node2 ->
          if (String.sub (to_string node) 1 ((String.length (to_string node))-1)) = 
            (String.sub (to_string node2) 1 ((String.length (to_string node2))-1))
          then Lts.add_transition lts2 "#" node node2
          else lts2
        )
        lts
        (NodeSet.filter
          (fun node2 ->
            (String.get (to_string node2) 0) == 'r'
          )
          (Lts.get_all_nodes lts)
        )
    )
    lts_read_write_trans
    (NodeSet.filter
      (fun node ->
        (String.get (to_string node) 0) == 'w'
      )
      (Lts.get_all_nodes lts_read_write_trans)
    )

let build_formula ?(v_lvl=V.None) transitions flush_mark =
 let open Formula in 
  let id1 = Lambda ("z1", Base, Var("z1",Base,NoFP)) in
  let id2 = Lambda ("z2", Base, Var("z2",Base,NoFP)) in
  let arg11 = Lambda ("x1", Base, Diamond("0", App((Var("f", Fun(Base,Base),NoFP)),(Var("x1",Base,NoFP))))) in
  let arg12 = Lambda ("y1", Base, Box("0", App((Var ("g", Fun(Base,Base),NoFP)), Var("y1",Base,NoFP)))) in
  let arg21 = Lambda ("x2", Base, App((Var("f", Fun(Base,Base),NoFP)),Diamond("1",Var("x2",Base,NoFP)))) in
  let arg22 = Lambda ("y2", Base, Box("1", App((Var ("g", Fun(Base,Base),NoFP)), Var("y2",Base,NoFP)))) in

  App(App(Nu("X", Fun(Fun(Base,Base),Fun(Fun(Base,Base),Base)), 
    Lambda("f", Fun(Base,Base), 
      (Lambda("g", Fun(Base,Base), 
          Conj(
            App(Var("g", Fun(Base,Base),NoFP), Box("#", App(Var("f", Fun(Base,Base),NoFP), Prop "END"))), 
            Conj(
              App(App(Var("X",Fun(Fun(Base,Base),Fun(Fun(Base,Base),Base)),GFP), arg11),arg12),
              App(App(Var("X",Fun(Fun(Base,Base),Fun(Fun(Base,Base),Base)),GFP), arg21),arg22)
            )
          )
          )
        )
      )
    ),id1),id2)

let _ =
  let formula = build_formula ~v_lvl:V.Info ["0";"1"] "#" in 
  let lts = build_lts ~zero_max_count:1 ~one_max_count:1 in
  let lts_broken = Lts.add_transition lts "0" (NamedNode "w0-1") (NamedNode "w0-0") in
  model_check ~v_lvl:V.Detailed formula lts;