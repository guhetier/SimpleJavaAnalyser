open Environment
open CustomMaps

module Make (Env: Environment) = struct

    open Simple_java_syntax

    module Field = Env.Field
    type state = Env.state

    let set_block_unreachable env blk =
        List.fold_left (fun env (_, ext) -> Env.setUnreachable env ext) env blk

    let interpret_var env var =
        Env.getValue env var

    let rec interpret_unary env op e =
        Field.unOp op (interpret_expr env e)

    and interpret_binary env op a b =
        Field.binOp op (interpret_expr env a) (interpret_expr env b)

    and interpret_expr env (expr, ext) =
        match expr with
        | Se_const c -> Field.convertVal c
        | Se_random (a,b) -> Field.convertInterval a b
        | Se_var var -> Env.getValue env var
        | Se_unary (op, e) -> interpret_unary env op e
        | Se_binary (op, a, b) -> interpret_binary env op a b

    let interpret_assign env ext var expr =
        let env = Env.recordState env ext in 
        Env.setValue env var (interpret_expr env expr)

    let rec interpret_condition env procs ext cond blk1 blk2 =
        let env = Env.recordState env ext in
        let v = interpret_expr env cond in
        let isTrue = Field.isValIn 1L v and isFalse = Field.isValIn 0L v in

        if isTrue && isFalse then
            let env1 = interpret_block env procs blk1
            and env2 = interpret_block env procs blk2 in

            (* Actualise outgoing env according to both branch *)
            Env.mergeEnv env1 env2

        else if isTrue && not isFalse then
            let env = set_block_unreachable env blk2 in
            interpret_block env procs blk1

        else (* isFalse && not isTrue*)
            let env = set_block_unreachable env blk1 in
            interpret_block env procs blk2

    and interpret_loop env procs ext cond blk =
        let env = Env.recordState env ext in

        let rec iterLoop env = 
            (* Executer la boucle *)
            let nextEnv = interpret_block env procs blk in
            if Env.isUnreachable nextEnv then
                nextEnv
            else
            (* - tester la condition *)
            let v = interpret_expr env cond in
            match Env.varToEnlarge env nextEnv with
            (* On a atteint un état stable*)
            | Some [] ->
                let vext = interpret_expr nextEnv cond in
                (* On boucle de manière certaine *)
                if(Field.isVal 1L vext) then
                    Env.unreachable nextEnv
                else
                    nextEnv
            (* Des variables ont évoluées *)
            | Some l ->
                (* On sort de la boucle en ne vérifiant plus la condition.
                 * On est donc dans l'état courant après la boucle *)
                (if Field.isVal 0L v then
                        nextEnv
                else
                    (* elargir les variables qui ont changés *)
                    let env = Env.enlarge nextEnv l in
                    iterLoop env
                )
            (* On découvre du nouveau code encore non atteint *)
            | None -> (if Field.isVal 0L v then
                    nextEnv
                else
                    iterLoop nextEnv)
        in

        let v = interpret_expr env cond in
        (* If we never enter the loop *)
        if (Field.isVal 0L v) then
            begin
            set_block_unreachable env blk;
            end
        else
            begin
            (* Else, try to execute 10 times the block : it may be enough *)
            let i = ref 10 and renv = ref env in
            while !i > 0 do
                renv := interpret_block !renv procs blk;
                if Env.isUnreachable !renv then(
                    renv := Env.unreachable (!renv);
                    i := -1
                )
                else(
                    let v = interpret_expr !renv cond in
                    i := !i - 1;
                    if Field.isVal 0L v then
                        i := -1
                )
            done;
            let env = !renv in
            (* If we don't go out of the loop in the first iterations*)
            if !i <> -1 then
                (* Then, try to operate by enlargement *)
                (* We need to have an env without record to know when we reach new
                 * code in an iteration *)
                begin
                    iterLoop (Env.clearRecords env) |> Env.mergeEnv env
                end
            else
                (* We go out of the loop *)
                env
            end

    and interpret_proc env procs ext proc =
        let env = Env.recordState env ext in
        try interpret_block env procs (Hashtbl.find procs proc)
        with Not_found -> failwith (Printf.sprintf "%s -> Undefined procedure %s::%s" (Localizing.extent_to_string ext) proc.s_proc_call_class proc.s_proc_call_name)

    and interpret_assert env ext expr =
        let env = Env.recordState env ext in
        let v = interpret_expr env expr in
        if Field.isVal 0L v then
            Env.unreachable env
        else
            env

    and interpret_command env procs (cmd, ext) =
        match cmd with
        | Sc_assign (var, exp)     -> interpret_assign env ext var exp
        | Sc_if (cond, blk1, blk2) -> interpret_condition env procs ext cond blk1 blk2
        | Sc_while (cond, blk)     -> interpret_loop env procs ext cond blk
        | Sc_proc_call proc        -> interpret_proc env procs ext proc
        | Sc_assert exp            -> interpret_assert env ext exp

    and interpret_block env procs blk =
        match blk with
        | [] -> env
        | c::q -> let env = interpret_command env procs c in
                    if not (Env.isUnreachable env) then
                        interpret_block env procs q
                    else 
                        set_block_unreachable env q

    (*
     * List and initialize variable declaration
     *)
    let interpret_var_decl env (var,init) =
        let env = Env.recordState env var.s_var_extent in
        match init with
        | None   -> Env.setValue env var Field.undef
        | Some e -> Env.setValue env var (interpret_expr env e)

    (*
     * List and store functions
     *)
    let interpret_proc_decl proc className p = 
        Hashtbl.replace proc
            {s_proc_call_class = className;
            s_proc_call_name = p.s_proc_name}
        p.s_proc_body

    (*
     * Interpret class definitions
     *)
    let interpret_class env proc c = 
        let rec readClassDeclaration env l =
            match l with
            | []    -> env
            | (Sd_var v)::q  -> let newEnv = interpret_var_decl env v in readClassDeclaration newEnv q
            | (Sd_function p)::q  -> interpret_proc_decl proc c.s_class_name p;
                                    readClassDeclaration env q
        in readClassDeclaration env c.s_class_body

    exception Found of s_block

    let interpret_program p =
        let procs = Hashtbl.create 10
        in
        (* Read declarations *)
        let env = List.fold_right (fun c env -> interpret_class env procs c) p Env.empty in

        (* Look for main function *)
        let body = try
            Hashtbl.iter (fun f body -> if f.s_proc_call_name = "main" then raise (Found body)) procs; []
        with Found body -> body
        in
        let finalEnv = interpret_block env procs body in
        let finalEnv = Env.recordState finalEnv (Localizing.extent_unknown()) in
        Printer.print_program_with_prop p (Env.recordToStrings finalEnv)
end
