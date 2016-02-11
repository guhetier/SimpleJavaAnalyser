open AbstractField

module type S =
    sig
        val interpret_program : Simple_java_syntax.s_program -> unit
    end

module Make (Field: AbstractField) : S = struct

    open Simple_java_syntax

    type state = (Simple_java_syntax.s_var, Field.t) Hashtbl.t

    type env = {
        vars    : state; (* List of all variable of the program and their current value at the point of execution *)
        proc    : (s_proc_call, s_block) Hashtbl.t; (* List of all procs of the program and their instructions *)
        records : (Localizing.extent, state) Hashtbl.t (* Possible values of all variable at a point af the program, with every access path *)
    }


    let stateToString  state =
        let ss = Hashtbl.fold (fun var value l -> (Printf.sprintf "{ %s : %s }" var.s_var_name (Field.toString value))::l) state [] in
        String.concat ", " ss

    let printRecords env =
        Hashtbl.iter (fun ext state -> Printf.printf "%s \n---\n %s \n---\n"
                                        (Localizing.extent_to_string ext)
                                        (stateToString state)) env.records
        

    let getLastVars env blk =
        let _, ext = List.hd(List.rev blk) in
        Hashtbl.find env.records ext

    let mergeStateChange env loc field = 
        if not (Hashtbl.mem env.records loc) then
            (Hashtbl.replace env.records loc (Hashtbl.copy field); true)
        else begin
            let prev = Hashtbl.find env.records loc in
            Hashtbl.fold (fun var vnew r ->
                            let vold = Hashtbl.find prev var in
                            let vmerge = Field.merge vold vnew in
                            if (not (Field.equal vmerge vold)) then
                                (Hashtbl.replace prev var vmerge; true)
                            else
                                r)
                        field
                        false
        end

    let mergeState env loc field =
        let _ = mergeStateChange env loc field in ()

    let mergeBlk env env2 =
        Hashtbl.fold
            (fun loc s2 r ->
                (mergeStateChange env loc s2) || r
            )
            env2.records
            false

    let intersectState env env2 =
        Hashtbl.iter (fun var v2 ->
            let v1 = Hashtbl.find env.vars var in
            Hashtbl.replace env.vars var (Field.intersect v1 v2))
            env2.vars
        

    let unreachableState env =
        let h = Hashtbl.create 10 in
        Hashtbl.iter (fun var _ -> Hashtbl.replace h var Field.unreach) env.vars;
        h

    let set_block_unreachable env blk =
        List.iter (fun (_, ext) -> mergeState env ext (unreachableState env); ()) blk

    let interpret_var env var =
        try Hashtbl.find env.vars var with Not_found -> Field.undef

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
        Hashtbl.replace env.vars var (interpret_expr env expr);
        mergeState env ext env.vars

    let rec interpret_condition env ext cond blk1 blk2 =
        let v = interpret_expr env cond in
        let isTrue = Field.isValIn 1L v and isFalse = Field.isValIn 0L v in
        let res = ref true in

        if isTrue && isFalse then
            begin
                let env2 = {
                    vars = Hashtbl.copy env.vars;
                    proc = env.proc;
                    records = env.records
                } in
                res := (interpret_block env blk1);
                res := (interpret_block env2 blk2) || (!res);

                (* Actualise outgoing env according to both branch *)
                intersectState env env2;
            end
        else if isTrue && not isFalse then
            begin
                set_block_unreachable env blk2;
                res := (interpret_block env blk1)
            end
        else (* isFalse && not isTrue*)
            begin
                set_block_unreachable env blk1;
                res := (interpret_block env blk2)
            end;
        mergeState env ext env.vars; !res

    and interpret_loop env ext cond blk =
        let rec iterLoop env2 = 
            interpret_block env2 blk; (* TODO : si le block ne fini pas... *)
            (* - tester la condition *)
            let v = interpret_expr env2 cond in
            if(not (mergeBlk env env2) || Field.isVal 0L v) then
                let lastVars = getLastVars env blk in
                let extEnv = {vars=lastVars; records=env.records; proc=env.proc} in
                let vext = interpret_expr extEnv cond in
                if(Field.isVal 1L vext) then
                begin
                    mergeState env ext (unreachableState env);
                    false
                end
                else
                begin
                    mergeBlk env2 env;
                    mergeState env ext lastVars;
                    true
                end
            else
                iterLoop env2
        in

        let v = interpret_expr env cond in
        if (Field.isVal 0L v) then
            begin
            set_block_unreachable env blk;
            mergeState env ext env.vars;
            true
            end
        else
            
            (* - tester le bloc *)
            let env2 = {
                proc = env.proc;
                vars = Hashtbl.copy env.vars;
                records = Hashtbl.create 10
                }
            in
            iterLoop env2
            (* - si encore possiblement vrai, tester le bloc a nouveau et merger les deux passages *)
            (* - elargir les variables qui ont changés *)
            (* - tant que le test peut être vrai et que des variables changent *)
            (* - la val du while est le merge de tout les états en tête de boucle *)

    and interpret_proc env ext proc =
        let res = (try interpret_block env (Hashtbl.find env.proc proc)
        with Not_found -> failwith (Printf.sprintf "%s -> Undefined procedure %s::%s" (Localizing.extent_to_string ext) proc.s_proc_call_class proc.s_proc_call_name)) in
        mergeState env ext env.vars;
        res

    and interpret_assert env ext expr =
        let v = interpret_expr env expr in
        if Field.isVal 0L v then
            (mergeState env ext (unreachableState env); false)
        else
            (mergeState env ext env.vars; true)

    and interpret_command env (cmd, ext) =
        match cmd with
        | Sc_assign (var, exp)     -> interpret_assign env ext var exp; true
        | Sc_if (cond, blk1, blk2) -> interpret_condition env ext cond blk1 blk2
        | Sc_while (cond, blk)     -> interpret_loop env ext cond blk
        | Sc_proc_call proc        -> interpret_proc env ext proc
        | Sc_assert exp            -> interpret_assert env ext exp

    and interpret_block env blk : bool =
        match blk with
        | [] -> true
        | c::q -> if interpret_command env c then
                        interpret_block env q
                else (set_block_unreachable env q; false)

    (*
     * List and initialize variable declaration
     *)
    let interpret_var_decl env (var,init) =
        (match init with
        | None   -> Hashtbl.replace env.vars var Field.undef
        | Some e -> Hashtbl.replace env.vars var (interpret_expr env e));
        mergeState env var.s_var_extent env.vars

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

    exception Found of s_block

    let interpret_program p =
        let env = {
            vars = Hashtbl.create 10; 
            proc = Hashtbl.create 10;
            records = Hashtbl.create 10;
        }
        in
        (* Read declarations *)
        List.iter (interpret_class env) p;


        (* Look for main function *)
        try
            Hashtbl.iter (fun f body -> if f.s_proc_call_name = "main" then raise (Found body)) env.proc
        with Found body -> let _ = interpret_block env body in
        let info = Hashtbl.create 10 in
        Hashtbl.iter (fun loc state -> Hashtbl.replace info loc (stateToString state)) env.records;
        Printer.print_program_with_prop p info
end
