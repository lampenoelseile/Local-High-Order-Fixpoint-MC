(* For high-level info about public parts see formulas.mli *)
open Basedata
open Tcsset

module Formula = struct
  type variable_t = 
  | Base
  | Fun of variable_t * variable_t

  type fp_t =
  | NoFP
  | LFP 
  | GFP

  let rec string_of_var_t = function
  | Base -> "base"
  | Fun(a,b) -> Format.sprintf "%s -> %s" (string_of_var_t a) (string_of_var_t b)
  type t = 
    | Const of bool
    | Prop of string
    | Var of string * variable_t * fp_t
    | Neg of t
    | Conj of t * t
    | Disj of t * t
    | Impl of t * t
    | Equiv of t * t
    | Diamond of string * t
    | Box of string * t
    | Mu of string * variable_t * t
    | Nu of string * variable_t * t 
    | Lambda of string * variable_t * t
    | App of t * t

  let to_string ?(max_length = 200) ?(show_types=false) phi = 
    let s_type id t = 
      if show_types then Format.sprintf "(%s:%s)" id (string_of_var_t t)
      else id in
    let rec f = function
      | Const(b) -> if b then "true" else "false"
      | Prop(prop_var) -> Format.sprintf "%s" prop_var
      | Var(var,t,fp) -> s_type var t
      | Neg(phi) -> Format.sprintf "~%s" (f phi) 
      | Conj(phi1, phi2) -> Format.sprintf "(%s && %s)" (f phi1) (f phi2) 
      | Disj(phi1, phi2) -> Format.sprintf "(%s || %s)" (f phi1) (f phi2) 
      | Impl(phi1, phi2) -> Format.sprintf "(%s => %s)" (f phi1) (f phi2) 
      | Equiv(phi1, phi2) -> Format.sprintf "(%s <=> %s)" (f phi1) (f phi2) 
      | Diamond(action, phi) -> Format.sprintf "<%s> %s" action (f phi) 
      | Box(action, phi) -> Format.sprintf "[%s] %s" action (f phi) 
      | Mu(var,t,phi) -> Format.sprintf "(mu %s. %s)" (s_type var t) (f phi) 
      | Nu(var,t,phi) -> Format.sprintf "(nu %s. %s)" (s_type var t) (f phi) 
      | Lambda(var,t,phi) -> Format.sprintf "(lam %s %s)" (s_type var t) (f_lambda phi)
      | App(phi,psi) -> Format.sprintf "(%s %s)" (f_app phi) (f psi)
    and f_lambda = function
      | Lambda(var,t,phi) ->
    Format.sprintf "%s %s" (s_type var t) (f_lambda phi)
      | _ as phi -> 
    Format.sprintf "-> %s" (f phi)
    and f_app = function
      | App(phi,psi) ->
    Format.sprintf "%s %s" (f_app phi) (f psi)
      | _ as phi -> f phi
    in 
    let value = f phi in
    let val_length = String.length value in
    if val_length <= max_length-3 then
      value
    else
    (String.sub value 0 (max_length / 2)) ^ " ... " 
    ^ (String.sub value (val_length - (max_length / 2)) (max_length/2))
end
  
module Semantics = struct
  type t =
    | Base of NodeSet.t
    | Fun of (t,t) TreeMap.t
  
  let rec to_string ?(max_length = 200) sem = 
    match sem with
    | Base(ns) -> NodeSet.to_string ns
    | Fun(map) -> let value = 
                    (TreeMap.fold 
                      (fun key value str -> str ^ "[" ^ to_string key ^ "->" ^ to_string value ^ "],") 
                      map "["
                    ) 
                    ^ "]"
                  in
                  let val_length = String.length value in
                  if val_length <= max_length-3 then
                    value
                  else
                  (String.sub value 0 (max_length / 2)) ^ " ... " 
                  ^ (String.sub value (val_length - (max_length / 2)) (max_length/2))


  
  let rec compare = function
    Base ns_one ->  (function
                  Base ns_two -> NodeSet.compare ns_one ns_two
                  | Fun map -> assert false)
    | Fun map_one -> (function
                      Base ns_two -> assert false
                      | Fun map_two -> let keys_one = List.sort (fun a b -> compare a b) (Tcsbasedata.Iterators.to_list 
                              (TreeMap.to_key_iterator map_one)) in
                              let keys_two = List.sort (fun a b -> compare a b) (Tcsbasedata.Iterators.to_list 
                              (TreeMap.to_key_iterator map_two)) in
                              let rec helper = function
                                h1 :: t1 -> (function 
                                            h2 :: t2 -> let value = compare (TreeMap.find h1 map_one) (TreeMap.find h2 map_two) 
                                                        in if value != 0 then value else helper t1 t2
                                          |  [] -> 1
                                          )
                              | [] -> (function 
                                        h :: t -> -1
                                      | [] -> 0
                                      ) 
                              in 
                              helper keys_one keys_two            
                      )

  let empty_base = Base(NodeSet.empty)
  let empty_fun = Fun(TreeMap.empty compare)

  let rec is_defined_for_args = function
    | Base(ns) -> (function
                  | [] -> true
                  | _ ->  assert false)
    | Fun(map) -> (function
                  | [] -> assert false
                  | h :: [] -> TreeMap.mem h map 
                  | h :: t ->  let value = TreeMap.mem h map in
                                value && is_defined_for_args (TreeMap.find h map) t)

  let rec get_value_for_args = function
    | Base(ns) -> (function
                  | [] -> Base(ns)
                  | _ -> assert false)
    | Fun(map) -> (function
                  | [] -> assert false
                  | h :: [] -> TreeMap.find h map
                  | h :: t -> get_value_for_args (TreeMap.find h map) t)

  let rec set_value_for_args value = function
    | Base(ns) -> (function
                    | [] -> value
                    | _ ->  assert false)
    | Fun(map) ->  (function
                    | [] ->  assert false
                    | h :: [] -> Fun(TreeMap.add h value map)
                    | h :: t -> if is_defined_for_args (Fun map) [h] then
                                  Fun(TreeMap.add h (set_value_for_args value (TreeMap.find h map) t) map)
                                else
                                  let new_map = empty_fun in 
                                  Fun(TreeMap.add h (set_value_for_args value new_map t) map))
  
  let rec from_list_of_pairs = function
    [] -> empty_fun
    | h :: t -> let (arg,value) = h in set_value_for_args value (from_list_of_pairs t) [arg]
  
  let rec get_defined_arguments = function
    Base(ns) -> [[]]
  | Fun(map) -> if TreeMap.is_empty map then []
                else begin
                  let args = Tcsbasedata.Iterators.to_list 
                              (TreeMap.to_key_iterator map) 
                  in
                  List.fold_left 
                    (fun list arg ->
                      (List.map 
                        (fun arg_list -> arg :: arg_list) 
                        (get_defined_arguments (get_value_for_args (Fun map) [arg]))
                      ) 
                      @ list
                    ) 
                    [] 
                    args
                end

end

(*SET OF SEMANTICS *)
module SemanticsSet = struct
  type t = Semantics.t TreeSet.t

  let empty = TreeSet.empty Semantics.compare
  let add = TreeSet.add
  let iter = TreeSet.iter
  let fold = TreeSet.fold
  let union = TreeSet.union
  let elements = TreeSet.elements

  let to_string sems =
    (fold (fun sem str -> match sem with
                            Semantics.Base(ns) -> str ^ Semantics.to_string (Base ns)^ ","
                          | Semantics.Fun(map) -> str ^ Semantics.to_string (Fun map)^ "," ) 
          sems "[") ^ "]"
  
    let rec all_of_type lts = function
      Formula.Base -> NodeSet.fold_subsets (fun ns sems -> add (Semantics.Base ns) sems) (Lts.get_all_nodes lts) empty
    | Formula.Fun(arg_t, val_t) -> let all_arguments = all_of_type lts arg_t in
                                   let all_values = all_of_type lts val_t in
                                   let rec helper arguments values pairs =
                                    match arguments with
                                      | [] -> empty
                                      | h :: [] -> List.fold_left (fun val_sems value -> add (Semantics.from_list_of_pairs ((h,value)::pairs)) val_sems) empty values
                                      | h :: t -> List.fold_left (fun val_sems value -> union val_sems (helper t values ((h, value) :: pairs))) empty values
                                   in
                                   helper (elements all_arguments) (elements all_values) [] (* TODO explain *)
end