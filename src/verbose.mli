exception Err of string

type lvl = Debug | Detailed | Info | None

val ( >= ) : lvl -> lvl -> bool

val console_out : lvl -> lvl -> (unit -> string) -> unit