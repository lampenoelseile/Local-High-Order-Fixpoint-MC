let range a b =
(* SEE https://ocaml.org/learn/tutorials/99problems.html*)
  let rec aux a b =
    if a > b then [] else a :: aux (a+1) b  in
  if a > b then List.rev (aux b a) else aux a b

let rand_select list n =
(* SEE https://ocaml.org/learn/tutorials/99problems.html*)
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n-1) t
  in
  let extract_rand list len =
    extract [] (Random.int len) list
  in
  let rec aux n acc list len =
    if n = 0 then acc else
      let picked, rest = extract_rand list len in
      aux (n-1) (picked :: acc) rest (len-1)
  in
  let len = List.length list in
  aux (min n len) [] list len

let rec all_pairs list =
(*returns list of all pairs of elements in list.*)
  match list with
    | [] -> []
    | h :: tl -> (h,h) :: (List.map (fun i -> (h,i)) tl) @ (List.map (fun i -> (i,h)) tl) @ all_pairs tl

let num_needed_bits int_value =
  if int_value < 0 then invalid_arg "num_needed_bits"
  else
    (int_of_float((log ((float) int_value)) /. (log 2.0))) + 1

let int_to_binary_list int_value = (*HSB first*)
  if int_value < 0 then invalid_arg "bin_of_int" else
  if int_value = 0 then [0] else
  let rec aux acc d =
    if d = 0 then acc else
    aux ((d land 1) :: acc) (d lsr 1)
  in
  aux [] int_value