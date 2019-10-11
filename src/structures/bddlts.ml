open MLBDD
open Tcsset

type t = {manager:MLBDD.man; propositions:(string,MLBDD.t)TreeMap.t ; transitions:(string,MLBDD.t)TreeMap.t}

let create_empty ?(proposed_size=1002403) = (*TODO: What does this size mean?*)
  {manager = MLBDD.init ~cache:proposed_size (); propositions = TreeMap.empty(String.compare); transitions = TreeMap.empty(String.compare)}

let amount_of_vars number_of_nodes = (int)((log (number_of_nodes*.1.0)) /. (log 2.0))

let create_dummy number_of_nodes = ()
