open Hfl

exception Err of string

type lvl = Debug | Detailed | Info | None

val ( >= ) : lvl -> lvl -> bool

val console_out : lvl -> lvl -> (unit -> string) -> unit

val duration_out : lvl -> lvl -> float -> float -> string -> unit

val sem_log_out : ?folderpath:string -> lvl -> lvl -> Semantics.t -> Formula.t -> unit