open Lexing
open Parsing


(** MISC **)
(** computes the list of sublists *)
let rec list_powerset = function
  | [] -> [[]]
  | a::l -> 
      let l2 = list_powerset l in
      (List.map (fun l-> a::l) l2)@l2

(** list the nth first terms of arith sequence x(n)=an+b**)
let arith_seq n a b = 
  let rec f res n = 
    if n = 0 then res else  f (a*(n-1)+b::res) (n-1) in
  f [] n

(** List.mapi as in version 4.00 of Ocaml) **)
let mapi f l =
  let l2 = List.combine (arith_seq (List.length l) 1 0) l in
  List.map (fun (i,x)-> f i x) l2

let rec list_setunion l1 = function
  | [] -> l1
  | a::l2 when List.mem a l1 -> list_setunion l1 l2
  | a::l2 -> list_setunion (a::l1) l2

let rec list_setminus l1 l2 = 
  match l1 with
  | [] -> []
  | a::ll1 when List.mem a l2 -> list_setminus ll1 l2
  | a::ll1 -> a::(list_setminus ll1 l2)


let list_cartesian_product l1 l2 = 
  let rec f res = function
    | [] -> res
    | a::ll2 -> f ((List.map (fun b-> (b,a)) l1)@res) ll2
  in f [] l2
 
let rec list_nth_product n l =
  if n=0 then [[||]]
  else let ll = list_nth_product (n-1) l in
  let f x a = 
    let b = Array.make ((Array.length a)+1) x in
    for i=0 to (Array.length a)-1 do
      b.(i+1)<-a.(i)
    done;
    b 
  in List.flatten (List.map (fun x->(List.map (f x) ll)) l)
  


(** Options file **)

let special_labels_list = ref []

let special_labels () = !special_labels_list

let load_options filename = 
  try
    let ch = open_in filename in
    try
      while true do
	let line = input_line ch in
	let a = Array.of_list (Str.split (Str.regexp " ") line) in
	special_labels_list := ((a.(2),a.(3)),a.(0)):: !special_labels_list;
      done
    with
    | _ -> close_in ch
  with _ -> ()





(** IDS **)
module Ids = struct
  type action = string
  type prop_var = string
  type pred_var = string
  type state = string

  let id_server () = 
    let i = ref 0 in
    let table1 = Hashtbl.create 100 in
    let table2 = Hashtbl.create 100 in
    let int_of_string str = 
      try Hashtbl.find table1 str
      with Not_found -> 
	Hashtbl.add table1 str !i;
      Hashtbl.add table2 !i str;
	i:= !i+1;
	!i-1 in
    let string_of_int n = Hashtbl.find table2 n in
    let record_string str = string_of_int (int_of_string str) in
    let all_ids () =
      let res = ref [] in 
      Hashtbl.iter (fun _ s -> res:=(s:: !res)) table2;
      !res in
    let num_ids () = !i in
    let identity x = x in
    (record_string,identity,int_of_string,string_of_int,num_ids,all_ids)

  let (action_of_string,string_of_action,int_of_action,action_of_int,nb_actions,all_actions) = id_server()
  let (prop_var_of_string,string_of_prop_var,int_of_prop_var,prop_var_of_int,nb_prop_vars,all_prop_vars) = id_server()
  let (state_of_string,string_of_state,int_of_state,state_of_int,nb_states,_) = id_server()

  let dummy_action () = string_of_action (action_of_string "unlabelled")
end


module Location = struct
  type t_record = 
      {file: string; line: int; start_char: int; end_char: int}

  type t = t_record option

  let none : t = None

  let mkloc s e =
    Some{file = e.pos_fname; 
	 line = s.pos_lnum;
	 start_char = s.pos_cnum - s.pos_bol;
	 end_char = e.pos_cnum - s.pos_bol}

  let symbol_loc () = 
    mkloc (symbol_start_pos()) (symbol_end_pos())

  let sprint loc = match loc with
  | Some{file=f; line=l; start_char=s; end_char=e} ->
      Format.sprintf "File \"%s\", line %i, characters %i-%i:@." f l s e
  | None -> ""

  let print fmt loc = Format.pp_print_string fmt (sprint loc)

  let lexbuf : lexbuf option ref = ref None
end

module Error = struct
  exception Illegal_character of char * Location.t
  exception Unterminated_comment of Location.t
  exception Parse_error of Location.t
  exception Unknown_identifier of string
  exception InitInvalidRank of int * int 
  exception InitInvalidState of Ids.state
  exception Type_error of string
  let report thunk = 
    try thunk () with
    | Illegal_character(c,loc) ->
	invalid_arg(
	(Location.sprint loc) ^ 
	(Format.sprintf "Syntax error: Illegal character (%s)@." 
	   (Char.escaped c)))
    | Unterminated_comment(loc) ->
	invalid_arg (
	(Location.sprint loc) ^ 
	(Format.sprintf "Syntax error: Unterminated comment@."))
    | Parse_error(loc) ->
	invalid_arg(
	(Location.sprint loc) ^ 
	(Format.sprintf "Parse error@."))
    | Unknown_identifier(s) ->
	invalid_arg(
	Format.sprintf "ERROR: unknown identifier \"%s\"@." s)
    | InitInvalidRank(given,expected) ->
	invalid_arg(
	Format.sprintf "ERROR: %d-tuple given as initial state, but formula has rank %d.@." given expected)
    | InitInvalidState(state) ->
	invalid_arg(
	Format.sprintf "ERROR: unknown initial state %s.@." (Ids.string_of_state state))
    | Type_error(s) ->
	invalid_arg(
	Format.sprintf "TYPE ERROR: %s.@." s)
end
