open Tcsset
open Hfl
  module S = Semantics
open Printf

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

let sem_log_out ?(folderpath="./outputs/") global_lvl message_lvl sem form =
  if global_lvl >= message_lvl then
    begin
      let rec helper sem path filename = 
        match sem with
          S.Base ns ->  let oc = open_out (folderpath ^ filename ^ ".log") in
                        fprintf oc "%s" (S.to_string sem);
                        close_out oc
        | S.Fun map ->  let arg_replace = ref 0 in 
                        let value_replace = ref 0 in 
                        let output =  (TreeMap.fold 
                                      (fun key value str ->
                                        match key, value with
                                          S.Base(key_ns), S.Base(value_ns) -> str ^ " [" ^ S.to_string key ^ "->" ^ S.to_string value ^ "]\n"
                                        | S.Fun(map), S.Base(ns) -> helper key path (filename ^ "_arg" ^ (string_of_int !arg_replace));
                                                                    arg_replace := !arg_replace + 1;
                                                                    str ^ " [arg" ^ (string_of_int (!arg_replace-1)) ^ "->" ^ S.to_string value ^ "]\n"
                                        | S.Base(ns), S.Fun(map) -> helper value path (filename ^ "_value" ^ (string_of_int !value_replace));
                                                                    value_replace := !value_replace + 1;
                                                                    str ^ " [" ^ S.to_string key ^ "->" ^ (string_of_int (!value_replace-1)) ^ "]\n"
                                        | S.Fun(key_map), S.Fun(value_map) -> helper key path (filename ^ "_arg" ^ (string_of_int !arg_replace));
                                                                              arg_replace := !arg_replace + 1;
                                                                              helper value path (filename ^ "_value" ^ (string_of_int !value_replace));
                                                                              value_replace := !value_replace + 1;
                                                                              str ^ " [" ^ (string_of_int (!arg_replace-1)) 
                                                                              ^ "->" ^ (string_of_int (!value_replace-1)) ^ "]\n"
                                      ) map "[\n") ^ "]"
                        in
                        let oc = open_out (folderpath ^ filename ^ ".log") in
                        fprintf oc "%s" output;
                        close_out oc
      in 
      helper sem folderpath (Formula.to_string form)
    end
  else
    ()