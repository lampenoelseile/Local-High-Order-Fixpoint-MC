(* TOOLDBOX *)

val range : int -> int -> int list

val rand_select : 'a list -> int -> 'a list

val all_pairs : 'a list -> ('a * 'a) list

val num_needed_bits : int -> int

val int_to_binary_list : int -> int list

val split_list_in_half : 'a list -> ('a list * 'a list)

val list_to_string : 'a list -> ('a -> string) -> string