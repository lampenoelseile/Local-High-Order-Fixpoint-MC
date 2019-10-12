open MLBDD
open Bddlts
open Mcbdd
open Tcsset
open Hfl
  module F = Formula
let _ =
let lts = Bddlts.add_proposition(Bddlts.add_proposition (create 11) "p" 2) "p" 10 in
let lts2 = Bddlts.add_transition (Bddlts.add_transition (Bddlts.add_proposition lts "q" 3) "u" 2 2) "u" 10 3 in
let lts3 = Bddlts.add_transition (Bddlts.add_transition lts2 "u" 9 4) "u" 4 9 in
let formula = F.Conj (F.Prop("p"), Diamond("u", Prop("q"))) in 
model_check formula lts3
