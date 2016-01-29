

open Simple_java_syntax

(*
 * Statically check for non initialized variables and type errors
 * Follow instruction order of the program and check for type and init at each step
 *)

(*
 * Program environement
 * var : var_id <-> s_var, is_var_init
 * proc : className, procName <-> proc instruction
 *)

type env = {
    var : (s_uniqueId, s_var) Hashtbl.t;
    proc : ((string*string), s_block) Hashtbl.t
}

(*
 * Exception management : fail with the error and localisation
 *)
exception Type_exception of string

let fail_message (err : string) (loc : Localizing.extent) =
    Printf.printf "Interpretation failed on %s.  ---  Error : %s\n"
    (Localizing.extent_to_string loc) err

(*
 * Diplay variables values
 *)
let print_variables env =
    let print_var id var =
        Printf.printf "%s (%s)" var.s_var_name (if var.s_var_type = St_bool then "bool" else "int");
        print_string "\n"
    in
    Hashtbl.iter print_var env.var


(*
 * Verifiy variable access
 *)
let verify_var env ext var = 
    var.s_var_type

(*
 * Verify unary operators
 *)
let rec verify_unary env ext op e = 
    match op with
    | Su_neg -> if  verify_expr env e <> St_bool then
        fail_message "Invalid type : St_bool expected" ext;
        St_bool
        
(*
 * Interpret binary operators TODO : reorganize operators
 *)
    and verify_binary env ext op a b =
    let compute_op op va vb =
        match va, vb with
        | St_int, St_int -> (match op with
            | Sb_add | Sb_sub | Sb_mul | Sb_div -> St_int 
            | Sb_lt  -> St_bool
            | _ -> failwith "Should not happen"
            )
        | _, _ -> fail_message "Invalid type : St_int*St_int expected" ext;
                if op = Sb_lt then St_bool else St_int
    in
    match op with
        | Sb_or -> (match (verify_expr env a), (verify_expr env b) with
            | (St_bool, St_bool) -> St_bool
            | _ -> fail_message "Invalid type : St_bool*St_bool expected" ext; St_bool
            )
        | _ -> compute_op op (verify_expr env a) (verify_expr env b)

(*
 * Interpret expressions
 *)
    and verify_expr env (expr, ext) = 
        match expr with
        | Se_const e           -> (match e with
            | Sc_bool _ -> St_bool
            | Sc_int _  -> St_int
            )
        | Se_random (a,b)      -> St_int
        | Se_var var           -> verify_var env ext var
        | Se_unary (op, e)     -> verify_unary env ext op e
        | Se_binary (op, a, b) -> verify_binary env ext op a b

(*
 * Interpret variable assignment
 *)
let verify_assign env var expr =
    if  verify_expr env expr <>  var.s_var_type then
        fail_message "Invalid type : variable incompatible with expression" (snd expr)

(* 
 * Interpret conditions
 *)
let rec verify_condition env cond blk1 blk2 =
    match verify_expr env cond with
    | St_bool -> verify_block env blk1;
                 verify_block env blk2
    | _       -> fail_message "Invalid type : St_bool expected" (snd cond)

(*
 * Interpret loops
 *)
and verify_loop env cond blk = 
    match verify_expr env cond with
    | St_bool -> verify_block env blk;
    | _       -> fail_message "Invalid type : St_bool expected" (snd cond)

(*
 * Interpret procedure call
 *)
and verify_proc env proc = 
    let p = (try Hashtbl.find env.proc (proc.s_proc_call_class,proc.s_proc_call_name)
    with Not_found -> raise (Type_exception "Undefined procedure"))
    in verify_block env p

(*
 * Interpret assert
 *)
and verify_assert env expr =
    if verify_expr env expr <> St_bool then
        fail_message "Invalid type : St_bool expected" (snd expr)

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
    | (cmd, loc)::q -> verify_command env cmd;
        verify_block env q

(*
 * List and initialize variable declaration
 *)
let verify_var_decl env (var,init) =
    Hashtbl.replace env.var var.s_var_uniqueId var;
    match init with
        | None   -> ()
        | Some e -> if verify_expr env e <> var.s_var_type then
                        fail_message (Printf.sprintf "Invalid init type : variable %s incompatible with expression type" var.s_var_name) var.s_var_extent
                        
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
    (* Create environement *)
    let env = {
        var  = Hashtbl.create 10;
        proc = Hashtbl.create 10} 
    in
    (* Read all declrations in the program *)
    let rec readDeclarations l = match l with
        | []   -> ()
        | h::q -> verify_class env h; readDeclarations q
in readDeclarations p;

    (* print variables at the begining of the exection *)
    print_endline "\nVariables :\n-----------";
    print_variables env;
    print_endline "------------\n";

    (* Look for function main and run it *)
    try
        Hashtbl.iter (fun (c, f) body -> if f = "main" then raise (Found body)) env.proc
    with Found body -> verify_block env body;






