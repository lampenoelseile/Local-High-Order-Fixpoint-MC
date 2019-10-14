open MLBDD
open Bddlts
open Mcbdd
open Tcsset
open Hfl
  module F = Formula
let _ =
let lts = Bddlts.add_proposition(Bddlts.add_proposition (create 20) "p" 2) "p" 9 in
let lts2 = Bddlts.add_transition (Bddlts.add_transition (Bddlts.add_proposition lts "q" 2) "u" 2 2) "u" 9 3 in
let lts3 = Bddlts.add_transition (Bddlts.add_transition lts2 "u" 9 4) "u" 4 2 in
let open F in 
let formula = App( Lambda("Y", Fun(Base,Base),(F.Disj (App(Var("Y",Fun(Base,Base),NoFP),(F.Prop("p"))), Box("u", Prop("q"))))) , F.Lambda("X", Base, Var("X",Base, NoFP))) in
let f = App(Lambda("Y", Fun(Base,Base), App(App(Var("Y", Fun(Base,Base),NoFP), Lambda("W", Base, Var("W",Base,NoFP))), Prop("p"))), 
(Lambda("Z", Fun(Base,Base), Var("Z",Fun(Base,Base),NoFP))) ) in

let f = App( Lambda("Y", Fun(Fun(Base,Base), Fun(Base,Base)), App((App(Var("Y",Fun(Fun(Base,Base), Fun(Base,Base)),NoFP), Lambda("X", Fun(Base,Base), Var("X", Fun(Base,Base), NoFP)))), Prop("p"))), 

Lambda("X", Fun(Base,Base), Var("X", Fun(Base,Base), NoFP))) in
let all = (HO.all_of_type lts3 Formula.Base) in
(*print_endline (Tools.list_to_string (all) (function HO.Base s -> MLBDD.to_string s | _ -> assert false));*)
model_check formula lts3
