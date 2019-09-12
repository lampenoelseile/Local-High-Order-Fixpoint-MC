exception Err of string
type lvl = All | Debug | Info | None
val ( >= ) : lvl -> lvl -> bool
val console_out : lvl -> lvl -> string -> unit
