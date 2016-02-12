open Simple_java_syntax

module VConstant = AbstractInterpret.Make(Constant.Constant)
module VInterval = AbstractInterpret.Make(Interval.Interval)

let main () =
    (* Parsing arguments *)
    let f_name = ref ""
    and print = ref false
    and interpret = ref false
    and verify  = ref false
    and vinit = ref false
    and vtype = ref false
    and interval = ref false
    and constant = ref false in
    Arg.parse [
        ("--print", Arg.Set(print), "Display program source");
        ("--interpret", Arg.Set(interpret), "Interpret program source");
        ("--verify", Arg.Set(verify), "Verify program source");
        ("--vinit", Arg.Set(vinit), "Verify variables are initialized");
        ("--vtype", Arg.Set(vtype), "Verify program typing");
        ("--interval", Arg.Set(interval), "Perform interval variable propagation");
        ("--constant",  Arg.Set(constant), "Perform constant variable propagation")
    ]
    (fun s -> f_name := s) "Mini-Java analyzer";

    (* Parsing of the source file *)
    if String.compare !f_name "" = 0 then failwith "no program file given";

    let f_desc = open_in !f_name in

    (* Print program code if asked *)
    (*if !displayCode then print_file f_desc;*)

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

    (* Print program code if asked *)
    if !print then Printer.print_program s_prog;

    (* interpret program if asked *)
    if !interpret then Interpreter.interpret_program s_prog;

    if !vinit then V_init.verify_program s_prog;

    if !vtype then V_type.verify_program s_prog;

    (* verify program if asked *)
    if !verify then Verifiyer.verify_program s_prog;

    if !constant then VConstant.interpret_program s_prog;

    if !interval then VInterval.interpret_program s_prog

  let _ = main ()
