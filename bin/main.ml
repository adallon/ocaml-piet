open Ocaml_piet

(*
let path = "/home/antoine/dépôts/perso/ocaml-piet/examples/alpha_filled.png"
*)
let version = "ocaml-piet version 0"

exception End

type config = 
  {mutable steps: bool; 
  mutable verbosity: int;
  mutable filename: string option}

let init_config = {steps = false ; verbosity = 0; filename = None}
let trace config () = 
    if (config.verbosity = 0)
    then config.verbosity <- 1
    else ()
let debug config () = 
    if (config.verbosity < 2)
    then config.verbosity <- 2
    else ()
let verbose config () =  config.verbosity <- 1000 (* max verbosity *)
let interactive config () = 
  let _ =
    config.steps <- true;
  in ()


let main () = 
  let argument_reader config =
    let spec_list =
      [
      ("-t", Arg.Unit (trace config), "Prints the execution trace. Same as -v 1");
      ("-i",Arg.Unit (interactive config), "Stops after each execution step. Useful for debugging. Automatically sets -t");
      ("--debug",Arg.Unit (debug config), "Print debugging information for ocaml-piet");
      ("--verbose",Arg.Unit (verbose config), "Maximal verbosity");
      ("--version", Arg.Unit (fun () -> print_string version ; raise End ), "Print version number" );
      ]
    in let usage_message = "./ocaml-piet.exe file [options]"
    in Arg.parse spec_list print_endline usage_message ;
    config
  in let config = argument_reader init_config
  in let _ =
    Util.set_verb_level config.verbosity ;
    Util.set_steps config.steps
  in ()

let path = Sys.argv.(1);;
let map = Program.of_png path;;
    (* Ocaml_piet.Program.codel_map_example  *)
(* print_string (Program.codel_map_to_string map);*)
let _ = main () in Machine.interpreter map;;
