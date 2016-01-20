
open Simple_java_syntax

(*
 * Statically check for non initialized variables and type errors
 *
 * Follow instruction order of the program and check for type and init at each step
 *)

(*
 * Program environement
 * var : var_id <-> s_var, is_var_init
 * proc : className, procName <-> proc instruction
 *)

type state = Sure | Maybe | Not

type env = {
    var : (s_uniqueId, s_var * state) Hashtbl.t;
    proc : ((string*string), s_block) Hashtbl.t
}

(*
 * Exception management : fail with the error and localisation
 *)
exception Interpretation_exception of string

let fail_message (err : string) (loc : Localizing.extent) =
    failwith (Printf.sprintf "Interpretation failed on %s.  ---  Error : %s"
    (Localizing.extent_to_string loc) err)

(*
 * Diplay variables values
 *)
let print_variables env =
    let print_var id (var, isInit) =
        Printf.printf "%s (%s) -> " var.s_var_name (if var.s_var_type = St_bool then "bool" else "int");
        print_string (match isInit with
        | Sure   -> "Initialized"
        | Maybe -> "Maybe non initialized"
        | Not   -> "Non initialized"
        );
        print_string "\n"
    in
    Hashtbl.iter print_var env.var

let merge_env_if env env1 env2 = 
    Hashtbl.iter (fun id (_, s1) ->
        let var, s = Hashtbl.find env.var id and _, s2 = Hashtbl.find env1.var id in
        let newState = match s, s1, s2 with
        | Sure, _, _
        | _, Sure, Sure -> Sure
        | Not, Not, Not -> Not
        | _ -> Maybe
        in Hashtbl.replace env.var id (var,newState)
    ) env1.var;
    print_variables env 

let merge_env_loop env env1 = 
    Hashtbl.iter (fun id (_, s1) ->
        let var, s = Hashtbl.find env.var id in
        let newState = match s, s1 with
        | Sure, _ -> Sure
        | Not, Not -> Not
        | _ -> Maybe
        in Hashtbl.replace env.var id (var,newState)
    ) env1.var

(*
 * Verifiy variable access
 *)
let verify_var env var = 
    let var, isInit =  (try Hashtbl.find env.var var.s_var_uniqueId
    with Not_found -> raise (Interpretation_exception (Printf.sprintf "Undeclared variable %s" var.s_var_name)))
    in (match isInit with
    | Sure -> ()
    | _      -> raise (Interpretation_exception (Printf.sprintf "Variable %s may not be initialized" var.s_var_name))
    );
    var.s_var_type

(*
 * Verify unary operators
 *)
let rec verify_unary env op e = 
    match op with
    | Su_neg -> let v = verify_expr env e in
    (match v with 
        | St_bool -> St_bool
        | _         -> raise (Interpretation_exception "Invalid type : St_bool expected")
        )
(*
 * Interpret binary operators
 *)
    and verify_binary env op a b =
    let compute_op op va vb =
        match va, vb with
        | St_int, St_int -> (match op with
            | Sb_add | Sb_sub | Sb_mul | Sb_div -> St_int 
            | Sb_lt  -> St_bool
            | _ -> failwith "Should not happen"
            )
        | _, _ -> raise (Interpretation_exception "Invalid type : St_int*St_int expected")
    in
    match op with
        | Sb_or -> (match (verify_expr env a), (verify_expr env b) with
            | (St_bool, St_bool) -> St_bool
            | _ -> raise (Interpretation_exception "Invalid type : St_bool*St_bool expected")
            )
        | _ -> compute_op op (verify_expr env a) (verify_expr env b)

(*
 * Interpret expressions
 *)
    and verify_expr env (expr, ext) = 
        try (
        match expr with
        | Se_const e           -> (match e with
            | Sc_bool _ -> St_bool
            | Sc_int _  -> St_int
            )
        | Se_random (a,b)      -> St_int
        | Se_var var           -> verify_var env var
        | Se_unary (op, e)     -> verify_unary env op e
        | Se_binary (op, a, b) -> verify_binary env op a b
        ) with Interpretation_exception e -> fail_message e ext

(*
 * Interpret variable assignment
 *)
let verify_assign env var expr =
    match verify_expr env expr, var.s_var_type with
    | St_bool, St_bool -> Hashtbl.replace env.var var.s_var_uniqueId (var, Sure)
    | St_int,  St_int  -> Hashtbl.replace env.var var.s_var_uniqueId (var, Sure)
    | _, _ -> raise (Interpretation_exception "Invalid type : variable incompatible with expression")

(* 
 * Interpret conditions
 *)
let rec verify_condition env cond blk1 blk2 =
    match verify_expr env cond with
    | St_bool -> let env1 = {proc = env.proc ; var = Hashtbl.copy env.var} and env2 = {proc = env.proc; var = Hashtbl.copy env.var} in
                    (* Execute both branch on different env *)
                    verify_block env1 blk1;
                    verify_block env2 blk2;

                    (* Merge envs according followings rules :
                        * env : Sure -> Sure (var initialized before test)
                        * env1 : Sure and env2 : Sure -> Sure (var initialized in both test cases)
                        * env : Not, env1 : Not, env2 : Not -> Not (var not initialized and not initialized in both test cases
                        * _ -> maybe (we already didn't know var state, we are not sure of what happen in one branch or only one branch perform initialization...
                        *)
                    merge_env_if env env1 env2

    | _       -> raise (Interpretation_exception "Invalid type : St_bool expected")

(*
 * Interpret loops : TODO : if first line of loop = assing x, and x used on the loop, for the loop, Sure. But after the loop, not sure.
 *)
and verify_loop env cond blk = 
    match verify_expr env cond with
    | St_bool -> let env1 = {proc = env.proc ; var = Hashtbl.copy env.var} in
                    verify_block env1 blk;

                    (* Merge envs according followings rules :
                        * env : Sure -> Sure
                        * env : Not, env1 : Not -> Not
                        * _ -> Maybe
                        *)
                    merge_env_loop env env1
    | _       -> raise (Interpretation_exception "Invalid type : St_bool expected")

(*
 * Interpret procedure call
 *)
and verify_proc env proc = 
    let p = (try Hashtbl.find env.proc (proc.s_proc_call_class,proc.s_proc_call_name)
    with Not_found -> raise (Interpretation_exception "Undefined procedure"))
    in verify_block env p

(*
 * Interpret assert
 *)
and verify_assert env expr =
    match verify_expr env expr with
    | St_bool -> ()
    | _       -> raise (Interpretation_exception "Invalid type : St_bool expected")

(*
 * Interpret instructions
 *)
and verify_command env cmd =
    match cmd with
    | Sc_assign (var, expr)    -> verify_assign env var expr
    | Sc_if (cond, blk1, blk2) -> verify_condition env cond blk1 blk2
    | Sc_while (cond, blk)     -> verify_loop env cond blk
    | Sc_proc_call proc        -> verify_proc env proc
    | Sc_assert expr           -> verify_assert env expr

(*
 * Interpret block of instructions
 *)
and verify_block env blk =
    match blk with
    | []            -> ()
    | (cmd, loc)::q -> (try verify_command env cmd
    with Interpretation_exception e -> fail_message e loc);
    verify_block env q

(*
 * List and initialize variable declaration
 *)
let verify_var_decl env (var,init) =
    match init with
    | None   -> Hashtbl.replace env.var var.s_var_uniqueId (var, Not)
    | Some e -> if verify_expr env e = var.s_var_type then
        Hashtbl.replace env.var var.s_var_uniqueId (var, Sure)
    else
        raise (Interpretation_exception (Printf.sprintf "Invalid init type : variable %s incompatible with expression type" var.s_var_name))
(*
 * List and store functions
 *)
let verify_proc_decl env className p = 
    Hashtbl.replace env.proc (className, p.s_proc_name) p.s_proc_body

(*
 * Interpret class definitions
 *)
let verify_class env c = 
    let rec readClassDeclaration l = match l with
    | []    -> ()
    | h::q  -> (match h with
        | Sd_var v      -> verify_var_decl env v
        | Sd_function p -> verify_proc_decl env c.s_class_name p
    );
    readClassDeclaration q
    in readClassDeclaration c.s_class_body


exception Found of s_block

(*
 * Interpret a program
 *)
let verify_program (p:s_program) : unit =
    (* Initialize random generator *)
    Random.self_init();
    let env = {
        var  = Hashtbl.create 10;
        proc = Hashtbl.create 10} 
    in
    (* Read all declration in the program *)
    let rec readDeclarations l = match l with
    | []   -> ()
    | h::q -> verify_class env h; readDeclarations q
    in readDeclarations p;

    (* print variables at the begining of the exection *)
    print_endline "\nBegin :\n-----------";
    print_variables env;
    print_endline "------------\n";

    (* Look for function main and run it *)
    try
        Hashtbl.iter (fun (c, f) body -> if f = "main" then raise (Found body)) env.proc
    with Found body -> verify_block env body;
    (* Print variables at the end of execution *)
    print_endline "\nEnd :\n-----------";
    print_variables env;
    print_endline "------------"





