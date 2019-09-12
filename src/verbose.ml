exception Err of string

type lvl =
  | All
  | Detailed
  | Info
  | None

let (>=) = function
  | All -> (function
            | All -> false
            | _ -> false)
  | Detailed -> (function
            | All | Detailed -> true
            | _ -> false)
  | Info -> (function
            | All | Detailed | Info -> true
            | _ -> false)
  | None -> function | _ -> true

let console_out global_lvl message_lvl message =
  if global_lvl >= message_lvl then print_endline (message ()) else ()
  