open Mc
open Hfl
open Lts
module V = Verbose
open Datastructures

let build_named_lts order transitions flush_mark =
let open Node in
  let rec write depth lts =
    match (order == depth) with
      true -> lts
    | false -> write (depth+1) (List.fold_left (fun lts2 symb -> 
                                          NodeSet.fold  (fun lts3 node ->
                                                          let node_name = to_string node in
                                                          if false then begin
                                                          Lts.add_transition 
                                                            (Lts.add_transition 
                                                            (Lts.add_node lts3 (NamedNode (node_name ^ symb))) 
                                                            symb (NamedNode node_name) (NamedNode (node_name ^ symb))) 
                                                            symb (NamedNode (node_name ^ symb)) 
                                                            (NamedNode (node_name ^ symb)) 
                                                          end else
                                                          Lts.add_transition 
                                                            (Lts.add_node lts3 (NamedNode (node_name ^ symb))) 
                                                            symb (NamedNode node_name) (NamedNode (node_name ^ symb))
                                                        ) 
                                          lts2 
                                          (NodeSet.filter (fun node -> let name = to_string node in 
                                                                  if not (String.equal name "wEps")
                                                                    && ((String.length name) - 1) = depth 
                                                                  then true else false) 
                                            (Lts.get_all_nodes lts2))
                                        )
                                    lts transitions)
  in
  let rec read depth lts =
    match (order == depth) with
      true -> lts
    | false -> write (depth+1) (List.fold_left (fun lts2 symb -> 
                                          NodeSet.fold  (fun lts3 node ->
                                                          let node_name_sub = String.sub 
                                                                                (to_string node) 1 ((String.length (to_string node))-1) 
                                                          in
                                                          Lts.add_transition 
                                                            (Lts.add_node lts3 (NamedNode ("r" ^ symb ^ node_name_sub))) 
                                                            symb (NamedNode ("r" ^ symb ^ node_name_sub)) (NamedNode ("r" ^ node_name_sub))
                                                        ) 
                                          lts2 
                                          (NodeSet.filter (fun node -> let name = to_string node in 
                                                                  if not (String.equal name "rEps")
                                                                    && (Char.equal (String.get name 0) 'r')
                                                                    && ((String.length name) - 1) = depth 
                                                                  then true else false) 
                                            (Lts.get_all_nodes lts2))
                                        )
                                    lts transitions)
  in
  let lts = ref (write 1 
                      (List.fold_left 
                        (fun lts symb -> 
                        if false then
                          Lts.add_transition 
                            (Lts.add_transition 
                              (Lts.add_node lts (NamedNode ("w" ^ symb))) symb (NamedNode "wEps") (NamedNode ("w" ^ symb)))
                            symb
                            (NamedNode ("w" ^ symb))
                            (NamedNode ("w" ^ symb))
                        else
                          Lts.add_transition (Lts.add_node lts (NamedNode ("w" ^ symb))) symb (NamedNode "wEps") (NamedNode ("w" ^ symb))
                        ) 
                      (Lts.add_node Lts.create_empty (NamedNode "wEps")) transitions)) 
  in
  lts := read 1 
                (List.fold_left 
                  (fun lts symb -> Lts.add_transition (Lts.add_node lts (NamedNode ("r" ^ symb))) symb (NamedNode ("r" ^ symb)) (NamedNode "rEps")) 
                (Lts.add_proposition (Lts.add_node !lts (NamedNode "rEps")) (NamedNode "rEps") "END") transitions);
  NodeSet.fold (fun lts node -> 
                  let name = to_string node in
                  Lts.add_transition lts flush_mark node (NamedNode ("r" ^ (String.sub name 1 ((String.length name)-1))))
                ) 
    !lts (NodeSet.filter (fun node -> 
                            let name = to_string node in 
                            if (Char.equal (String.get name 0) 'w')
                            then true else false
                          ) 
          (Lts.get_all_nodes !lts))

let build_formula ?(v_lvl=V.None) transitions flush_mark =
  let open Formula in
  let f = Var("f",Fun(Base,Base)) in 
  let g = Var("g",Fun(Base,Base)) in
  let psi = List.fold_left  (fun form1 trans1 ->
                              let new_part =
                                Disj( 
                                  Disj(
                                    List.fold_left  (fun form2 trans2 ->
                                                      let tmp =
                                                        App(
                                                          f,
                                                          Diamond(
                                                            trans1,
                                                            App(
                                                              g,
                                                              Diamond(
                                                                flush_mark,
                                                                App(
                                                                  f,
                                                                  Diamond(
                                                                    trans2,
                                                                    App(
                                                                      g,
                                                                      Prop("END"))))))))
                                                      in
                                                      match form2 with
                                                      | Const b -> tmp
                                                      | _ -> Disj(form2,tmp)
                                                    )
                                    (Const false) (List.filter (fun elem -> elem != trans1) transitions)
                                    ,
                                    App(
                                      f,
                                      Diamond(
                                        trans1,
                                        App(
                                          g,
                                          Diamond(
                                            flush_mark,
                                            App(
                                              f,
                                                  Prop("END"))))))
                                  ),
                                  App(
                                      f,
                                      Diamond(
                                        flush_mark,
                                        App(
                                          f,
                                          Diamond(
                                            trans1,
                                            App(
                                              g,
                                                  Prop("END"))))))
                                )
                              in
                              match form1 with
                              | Const b -> new_part
                              | _ -> Disj(form1,new_part)
                            ) 
              (Const false) transitions 
    in
    let phi2 = List.fold_left (fun form trans ->
                                let tmp = App 
                                            (Var("X", Fun(Fun(Base,Base),Base)),
                                            Lambda("z",Base,
                                                    App(
                                                      f,
                                                      Diamond(trans, Var("z",Base))
                                                    )
                                                  )
                                          )
                                in
                                match form with
                                | Const b -> tmp
                                | _ -> Disj (form,tmp)  
                              ) 
                (Const false) transitions
    in
    let phi = Mu("X",Fun(Fun(Base,Base),Base),
                Lambda("f",Fun(Base,Base),
                  Disj(
                    psi,
                    phi2
                  )
                )
              )
    in
    V.console_out v_lvl V.Info (fun () -> "Formula PSI:\n" ^ to_string ~max_length:200 ~show_types:false psi ^ "\n");
    V.console_out v_lvl V.Info (fun () -> "Formula PHI:\n" ^ to_string ~max_length:200 ~show_types:false (Mu("X",Fun(Fun(Base,Base),Base),
                Lambda("f",Fun(Base,Base),
                  Disj(
                    Prop "PSI",
                    phi2
                  )
                )
              ))^ "\n");
    let complete = 
    Neg(
      App(
        Lambda("g",Fun(Base,Base),
          App(
            phi,
            Lambda("x",Base,Var("x",Base))
          )
        ),
        Lambda("y", Base,
          Mu("Y",Base,
            Disj(
              Var("y", Base),
              (List.fold_left  (fun form trans ->
                                let tmp = Diamond(trans, Var("Y",Base)) in
                                match form with
                                | Const b -> tmp
                                | _ -> Disj(form,tmp)
                              )
              (Const false) transitions)
            )
          )
        )
      )
    ) in 
    V.console_out v_lvl V.Info (fun () -> "Formula COMPLETE:\n" ^ to_string ~max_length:200 ~show_types:false (Neg(
      App(
        Lambda("g",Fun(Base,Base),
          App(
            Prop "PHI",
            Lambda("x",Base,Var("x",Base))
          )
        ),
        Lambda("y", Base,
          Mu("Y",Base,
            Disj(
              Var("y", Base),
              (List.fold_left  (fun form trans ->
                                let tmp = Diamond(trans, Var("Y",Base)) in
                                match form with
                                | Const b -> tmp
                                | _ -> Disj(form,tmp)
                              )
              (Const false) transitions)
            )
          )
        )
      )
    ) ) ^ "\n");
    complete


let _ =
  let formula = build_formula ~v_lvl:V.Info ["0";"1"] "#" in 
  let lts = build_named_lts 1 ["0";"1"] "#" in
  let lts_broken = Lts.add_transition lts "0" (Node.NamedNode "r0") (Node.NamedNode ("r1")) 
  in
  model_check ~v_lvl:V.Info formula lts;
  print_endline "\n";
  model_check ~v_lvl:V.Info formula lts_broken;