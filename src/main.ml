open Formulas
open Datastructures
open Lts

let toolname = "HFL^K-MC"
let helpmessage = 
  toolname ^
  ": a model-checker for higher-order fixpoint logic\n" ^
  "USAGE: " ^ toolname ^ " [options] formula.fml lts.dot\n"
let initial_tuple_string = ref ""
let save_dot = ref ""
let options_file = ref ".modml_options"
let use_hors = ref(false) 
let options =   (* complete that when we really need options *)
  ["-init",Arg.Set_string(initial_tuple_string),"\"state1,...,staten\" specifies the initial (tuple of) state(s)";
   "-save_dot",Arg.Set_string(save_dot),"file.dot saves the parsed lts in file.dot";
   "-load_options",Arg.Set_string(options_file),"FILE load options from FILE instead of " ^ !options_file;
   "-hors",Arg.Set(use_hors),"use translation to hors";
 ]
  

let parsee_command_line () =
  let formula_str = ref("") in
  let lts_str = ref("") in
  let f str = 
    if Str.last_chars str 3 = "fml" then formula_str:=str;
    if Str.last_chars str 3 = "dot" then lts_str:=str in
  Arg.parse options f helpmessage;
  if (!formula_str="") || (!lts_str="")
  then begin
    Format.print_string helpmessage;
    exit 1
  end;
  !formula_str,!lts_str

let _ =
  let f = App(Lambda("X", Fun(Base, Base), Equiv( App(Var("X", Fun(Base, Base)), Prop("p")), Const(false))), Lambda("Y", Base, Var("Y", Base))) in
  let lts = ref (Lts.create_random 20 ["a"; "b"; "c"] ["p"; "q"; "r"; "s"] 0.1 0.8) in
  Lts.print !lts;
  print_endline (string_of_formula ~show_types:false f);