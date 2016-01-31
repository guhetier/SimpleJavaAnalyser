open AbstractField

module AbstractInterpret (Field: AbstractField) = struct

    open Simple_java_syntax

    type env = {
        vars : Field.field;
        proc : (s_proc_call, s_block) Hashtbl.t
    }

    let set_block_unreachable env blk =
        failwith "TODO"

    let interpret_var env var =
        Field.getVar env.vars var

    let rec interpret_unary env op e =
        Field.unOp op (interpret_expr env e)

    and interpret_binary env op a b =
        Field.binOp op (interpret_expr env a) (interpret_expr env b)

    and interpret_expr env (expr, ext) =
        match expr with
        | Se_const c -> Field.convertVal c
        | Se_random (a,b) -> Field.convertInterval a b
        | Se_var var -> interpret_var env var
        | Se_unary (op, e) -> interpret_unary env op e
        | Se_binary (op, a, b) -> interpret_binary env op a b

    let interpret_assign env ext var expr =
        Field.setVar env.vars var (interpret_expr env expr)

    let rec interpret_condition env ext cond blk1 blk2 =
        failwith "TODO"

    and interpret_loop env ext cond blk =
        failwith "TODO"

    and interpret_proc env ext proc =
        try interpret_block env (Hashtbl.find env.proc proc)
        with Not_found -> failwith (Printf.sprintf "%s -> Undefined procedure %s::%s" (Localizing.extent_to_string ext) proc.s_proc_call_class proc.s_proc_call_name)

    and interpret_assert env ext expr =
        let v = interpret_expr env expr in
        not (Field.isVal 0L v)

    and interpret_command env (cmd, ext) =
        match cmd with
        | Sc_assign (var, exp)     -> interpret_assign env ext var exp; true
        | Sc_if (cond, blk1, blk2) -> interpret_condition env ext cond blk1 blk2; true
        | Sc_while (cond, blk)     -> interpret_loop env ext cond blk; true
        | Sc_proc_call proc        -> interpret_proc env ext proc; true
        | Sc_assert exp            -> interpret_assert env ext exp

    and interpret_block env blk =
        match blk with
        | [] -> ()
        | c::q -> if interpret_command env c then
                        interpret_block env q
                else set_block_unreachable env q

    (*
     * List and initialize variable declaration
     *)
    let interpret_var_decl env (var,init) =
        match init with
        | None   -> Field.addNonInitVar env.vars var
        | Some e -> Field.setVar env.vars var (interpret_expr env e)

    (*
     * List and store functions
     *)
    let interpret_proc_decl env className p = 
        Hashtbl.replace env.proc
            {s_proc_call_class = className;
            s_proc_call_name = p.s_proc_name}
        p.s_proc_body

    (*
     * Interpret class definitions
     *)
    let interpret_class env c = 
        let rec readClassDeclaration l =
            match l with
            | []    -> ()
            | (Sd_var v)::q  -> interpret_var_decl env v; readClassDeclaration q
            | (Sd_function p)::q  -> interpret_proc_decl env c.s_class_name p;
                                    readClassDeclaration q
        in readClassDeclaration c.s_class_body

    let rec readDeclarations env p =
        match p with
        | []   -> ()
        | h::q -> interpret_class env h; readDeclarations env q


    exception Found of s_block

    let interpret_program p =
        let env = {
            vars = Field.makeField (); 
            proc = Hashtbl.create 10
        }
        in
        readDeclarations env p;
        
        try
            Hashtbl.iter (fun f body -> if f.s_proc_call_name = "main" then raise (Found body)) env.proc
        with Found body -> interpret_block env body;
        ()


end
