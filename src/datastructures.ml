(** For detailed information see lts.mli *)
open Tcsbasedata
open Tcsset

(* NODE *)
module Node = struct
  type t =
  | SimpleNode of int
  | NamedNode of string

  let to_string = function
  SimpleNode i -> string_of_int i
  | NamedNode s -> s

  let compare = function
  SimpleNode i -> (function
              SimpleNode j -> Int.compare i j
            | NamedNode t -> assert false)
  | NamedNode s -> (function
                      SimpleNode j -> assert false
                    | NamedNode t -> String.compare s t)
end 




(* NODESET *)
module NodeSet = struct
  type t = Node.t TreeSet.t

  let isEmpty = TreeSet.is_empty
  let compare = TreeSet.compare
  let empty = TreeSet.empty Node.compare
  let elem = TreeSet.mem
  let fold f acc ns = TreeSet.fold (fun x y -> f y x) ns acc
  let iter = TreeSet.iter
  let filter = TreeSet.filter
  let map = TreeSet.map
  let size = TreeSet.cardinal
  let exists = TreeSet.exists
  let forall = TreeSet.for_all
  let first = TreeSet.min_elt
  let last = TreeSet.max_elt
  let some = TreeSet.choose
  let add = TreeSet.add
  let del = TreeSet.remove
  let union = TreeSet.union
  let inter = TreeSet.inter
  let diff = TreeSet.diff
  let of_node_list = TreeSet.of_list Node.compare
  let as_node_list = TreeSet.elements
  let fold_subsets = TreeSet.fold_subsets

  let find f ns =
      OptionUtils.get_some (fold (fun a v -> if a = None && f v then Some v else a) None ns)

  let some ws =
    let n = size ws in
    let i = ref (Random.int n) in
    find (fun v ->
      decr i;
      !i = -1
    ) ws

  let max ns lessf = fold (fun v -> fun w -> if lessf v w then w else v) (some ns) ns

  let to_string ns = "["^(String.concat "," (List.map Node.to_string (as_node_list ns)))^"]"

  let init =
    let rec f ns = function
      | 0 -> add (Node.SimpleNode 0) ns
      | c -> f (add (Node.SimpleNode c) ns) (c-1) in
      f empty
end

(* PROPOSITION SET *)
module PropSet = struct
  type t = string TreeSet.t

  let empty = TreeSet.empty String.compare
  let add = TreeSet.add 
  let del = TreeSet.remove 
  let of_string_list = TreeSet.of_list String.compare
  let as_string_list = TreeSet.elements
  let union = TreeSet.union

  let to_string ps = "["^(String.concat "," (as_string_list ps))^"]"
end
