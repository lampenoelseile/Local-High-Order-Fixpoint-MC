open MLBDD
open Bddlts
open Tcsset
let _ =
let bdd_manager = init ~cache:10 () in 
let var0 = ithvar bdd_manager 0 in 
let var1 = ithvar bdd_manager 1 in
let war0 = ithvar bdd_manager 2 in 
let war1 = ithvar bdd_manager 3 in 
let t = dand var0 var1 in 
let t_n = dnot t in
let t_or = dor t_n t_n in

let trans =  dor  (dand (dand (dnot var0) (var1)) (dand (war0) war1))
(dor (dand (dand (dnot var0) (dnot var1)) (dand (dnot war0) war1))
(dand (dand (dnot var0) (var1)) (dand (dnot war0) war1)))

in
let lts = Bddlts.add_proposition(Bddlts.add_proposition (create 11) "p" 2) "p" 10 in
let lts2 = Bddlts.add_transition (Bddlts.add_transition (Bddlts.add_proposition lts "q" 3) "u" 2 2) "u" 10 3 in

print_endline (Bddlts.to_string lts2);
print_endline (MLBDD.to_string(MLBDD.exists (Bddlts.get_tovars_support lts2) (MLBDD.dand (Bddlts.get_trans lts2 "u") (Bddlts.get_prop lts2 "p"))))
