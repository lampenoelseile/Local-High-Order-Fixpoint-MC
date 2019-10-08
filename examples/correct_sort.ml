open Mc
open Hfl
open Lts
module V = Verbose
open Datastructures

let build_lts ~zero_max_count ~one_max_count =
  let open Node in 
  let rec build list z_count o_count =
    match (z_count, o_count) with
      (0,0) ->  (NamedNode "i0-0") :: list
    | (z,0) ->  build ((NamedNode ("i" ^ (string_of_int z) ^ "-0")) :: list) (z-1) 0
    | (0,o) -> build ((NamedNode ("i" ^ "0-" ^(string_of_int o))) :: list) 0 (o-1)
    | (z,o) -> build ((NamedNode ("i" ^ (string_of_int z) ^ "-" ^(string_of_int o))) :: list) (z-1) (o-1) @ 
              build ((NamedNode ("i" ^ (string_of_int z) ^ "-" ^(string_of_int o))) :: list) (z) (o-1) @
              build ((NamedNode ("i" ^ (string_of_int z) ^ "-" ^(string_of_int o))) :: list) (z-1) (o)
  in
  let lts_init = List.fold_left (fun lts node -> Lts.add_node lts node) Lts.create_empty (build [] zero_max_count one_max_count) in
  (*NodeSet.fold 
    (fun lts node -> 
      let node_z_count_with_i = List.nth (String.split_on_char '-' (to_string node)) 0 in
      let node_o_count = List.nth (String.split_on_char '-' (to_string node)) 1 in
      let lts_new_z = 
        NodeSet.fold (fun lts2 node2 ->) 
        lts 
        (NodeSet.filter 
          (fun node2 ->) 
          lts
        )
      in
    ) 
    lts_init (Lts.get_all_nodes lts_init)*)
    lts_init


let build_formula ?(v_lvl=V.None) transitions flush_mark =
()


let _ =
  let formula = build_formula ~v_lvl:V.Info ["0";"1"] "#" in 
  let lts = build_lts ~zero_max_count:0 ~one_max_count:4 in
  print_endline (Lts.to_string lts)