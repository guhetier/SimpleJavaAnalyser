open Simple_java_syntax

(* Pretty print a program file *)
let print_file file = 
    let nline = ref 1 in
    try
        while true do
            Printf.printf "%i | %s\n" (!nline) (input_line file);
            nline := !nline + 1
        done
    with End_of_file -> seek_in file 0

let main () =
    (* Parsing arguments *)
    let f_name = ref "" and displayCode = ref false and interpret = ref false in
    Arg.parse [
        ("--display", Arg.Set(displayCode), "Display program source");
        ("--interpret", Arg.Set(interpret), "Interpret program source")
    ]
    (fun s -> f_name := s) "Mini-Java analyzer";

    (* Parsing of the source file *)
    if String.compare !f_name "" = 0 then failwith "no program file given";

    let f_desc = open_in !f_name in

    (* Print program code if asked *)
    if !displayCode then print_file f_desc;

    (* Parse and build an AST *)
    Localizing.current_file_name := !f_name;
    let lexbuf = Lexing.from_channel f_desc in
    let java_prog =
        try Java_parser.program Java_lexer.token lexbuf
      with
      | e -> 
              Printf.printf "Exception during parsing: %s\n" (Printexc.to_string e);
          failwith "Stopped"
    in
    let s_prog = Simple_java_translate.tr_java_prog java_prog in

    (* interpret program if asked *)
    if !interpret then Interpreter.interpret_program s_prog;

  Printf.printf "finished...\n"

  let _ = main ()
