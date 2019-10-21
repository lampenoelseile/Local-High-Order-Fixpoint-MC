(* TOOLDBOX *)

val range : int -> int -> int list
(** Returns range of integers (includes bounds).
    @param int lower bound 
    @param int upper bound
    @return range of integers
*)

val rand_select : 'a list -> int -> 'a list
(** Selects random object from list. 
    @param 'a list list to sample from
    @param int amount of samples
    @return samples
*)

val all_pairs : 'a list -> ('a * 'a) list
(** Pairs all elements in a list 
    @param 'a list list of n objects 
    @return list of (n x n) objects
*)

val num_needed_bits : int -> int
(** Returns number of needed bits to represent 
    given positive integer. 
    @param int integer 
    @param amount of bits
*)

val int_to_binary_list : int -> int list
(** Returns binary representation of integer, as
    list of 0's and 1's. 
    @param int integer to represent binary 
    @return binary representation in form of a list 
*)

val split_list_in_half : 'a list -> ('a list * 'a list)
(** Cuts list of even length in half. 
    @param 'a list list to split 
    @return pair of lists (first and second half)
*)

val strip_last_char : string -> string
(** Cuts last char of a string.
    @param string original string 
    @return strin (without last char)
*)
val list_to_string : 'a list -> ('a -> string) -> string
(** Helper: Printing list of objects. 
    @param 'a list list of objects 
    @param ('a -> string) to_string function for 'a objects
    @return string representation of list
*)