exception Err of string

type lvl = All | Detailed | Info | None

val ( >= ) : lvl -> lvl -> bool

val console_out : lvl -> lvl -> (unit -> string) -> unit
