open Ocaml_piet

(*
let path = "/home/antoine/dépôts/perso/ocaml-piet/examples/alpha_filled.png"
*)

(** Version number *)
let version = "ocaml-piet version 0.1"

(** exception allowing to end the execution of the main function *)
exception End

(** configuration representing the option passed as parameters of the executable 
 * @option steps activates the interactive step-by-step mode
   * The step by step mode is at least at verbosity level
 * @option verbosity represents the verbosity level:
   * 0 prints nothing
   * 1 prints the execution trace
   * above levels print more informations
 * @option filename is the filename of the file
 *)

type config = 
  {mutable steps: bool; 
  mutable verbosity: int;
  mutable filename: string option}

(**
 * init_config is the default configuration: 
   * non interactive
   * not verbose
 *)
let init_config = {steps = false ; verbosity = 0; filename = None}

(**
 * activates the trace output
 *)
let trace config () = 
    if (config.verbosity = 0)
    then config.verbosity <- 1
    else ()

(**
 * activates the debugging information
 *)
let debug config () = 
    if (config.verbosity < 2)
    then config.verbosity <- 2
    else ()

(**
 * activates the maximal verbosity
 *)
let verbose config () =  config.verbosity <- 1000 (* max verbosity *)

(**
 * activates the step by step mode
 *)
let interactive config () = 
  let _ =
    config.steps <- true;
  in ()

(**
 * option handler
 * options have been described above
 *)
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

  (**
   * file should be the first argument
   *)
  (**
   * computes the program from the image
   *)
    (* Ocaml_piet.Program.codel_map_example  *)
(* print_string (Program.codel_map_to_string map);*)

(**
  * Finds the path, defines the program, handles the options and launches the interpreter
  *)
let _ =
 try 
  let path = 
    try Sys.argv.(1) with
    | _ -> print_endline "Please enter a filename with an image describing a piet program"; raise End
  in let prog = Program.of_png path
  in let _ = main () in Machine.interpreter prog
  with
  | End -> ()
