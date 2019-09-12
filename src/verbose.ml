exception Err of string

type lvl =
  | All
  | Debug
  | Info
  | None

let (>=) = function
  | All -> (function
            | All -> false
            | _ -> false)
  | Debug -> (function
            | All | Debug -> true
            | _ -> false)
  | Info -> (function
            | All | Debug | Info -> true
            | _ -> false)
  | None -> function | _ -> true

let console_out global_lvl message_lvl message =
  if global_lvl >= message_lvl then print_endline message else ()
  