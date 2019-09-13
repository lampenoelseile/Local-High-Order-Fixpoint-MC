exception Err of string

type lvl =
  | Debug
  | Detailed
  | Info
  | None

let (>=) = function
  | Debug -> (function
            | Debug -> true
            | _ -> false)
  | Detailed -> (function
            | Debug | Detailed -> true
            | _ -> false)
  | Info -> (function
            | Debug | Detailed | Info -> true
            | _ -> false)
  | None -> function | _ -> true

let console_out global_lvl message_lvl message =
  if global_lvl >= message_lvl then print_endline (message ()) else ()
  