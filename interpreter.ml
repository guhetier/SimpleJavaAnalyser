open Simple_java_syntax

(*
 * Interpret a simple Java programm from Syntax tree
 *)

(*
 * Program environement
 * var : s_var <-> actual value of the variable
 * proc : className, procName <-> proc instruction
 *)
type env = {
    var : (s_uniqueId, s_constant option * string) Hashtbl.t;
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
let print_variables env prog =
    let print_var id (value, name) =
        Printf.printf "%s -> " name;
        (match value with
        | None            -> print_string "Undefined"
        | Some(Sc_bool b) -> print_string (if b then "True" else "False")
        | Some(Sc_int i)  -> print_int (Int64.to_int i));
        print_string "\n"
    in
    Hashtbl.iter print_var env.var

(*
 * Interpret variable access
 *)
let interpret_var env var = 
    match (try Hashtbl.find env.var var.s_var_uniqueId
    with Not_found -> raise (Interpretation_exception (Printf.sprintf "Undeclared variable %s" var.s_var_name)))
    with
    | Some v, _ -> v
    | None, _   -> raise (Interpretation_exception (Printf.sprintf "Non initialized variable %s" var.s_var_name))
                        
(*
 * Interpret unary operators
 *)
let rec interpret_unary env op e = 
    match op with
    | Su_neg -> let v = interpret_expr env e in
    (match v with 
        | Sc_bool b -> Sc_bool (not b)
        | _         -> raise (Interpretation_exception "Invalid type : St_bool expected")
        )
(*
 * Interpret binary operators
 *)
    and interpret_binary env op a b =
    let compute_op op va vb =
        match va, vb with
        | Sc_int ia, Sc_int ib -> (match op with
            | Sb_add -> Sc_int(Int64.add ia ib)
            | Sb_sub -> Sc_int(Int64.sub ia ib)
            | Sb_mul -> Sc_int(Int64.mul ia ib)
            | Sb_div -> if ib = Int64.zero
                        then raise (Interpretation_exception "Invalid operation : division by zero");
                        Sc_int(Int64.div ia ib)
            | Sb_lt  -> Sc_bool(ia < ib)
            | _ -> failwith "Should not happen"
            )
        | _, _ -> raise (Interpretation_exception "Invalid type : St_int*St_int expected")
    in
    match op with
        | Sb_or                  -> (match interpret_expr env a with
            | Sc_bool b1 when b1 -> Sc_bool true
            | Sc_bool b1         -> (match interpret_expr env b with
                | Sc_bool b2 -> Sc_bool b2
                | _          -> raise (Interpretation_exception "Invalid type : St_bool*St_bool expected")
                )
            | _ -> raise (Interpretation_exception "Invalid type : St_bool*St_bool expected")
            )
        | _ -> compute_op op (interpret_expr env a) (interpret_expr env b)

(*
 * Interpret expressions
 *)
    and interpret_expr env (expr, ext) = 
        try (
        match expr with
        | Se_const e           -> e
        | Se_random (a,b)      -> Sc_int(Int64.add a (Random.int64 (Int64.sub b a)))
        | Se_var var           -> interpret_var env var
        | Se_unary (op, e)     -> interpret_unary env op e
        | Se_binary (op, a, b) -> interpret_binary env op a b
        ) with Interpretation_exception e -> fail_message e ext

(*
 * Interpret variable assignment
 *)
let interpret_assign env var expr =
    match interpret_expr env expr, var.s_var_type with
    | Sc_bool b, St_bool -> Hashtbl.replace env.var var.s_var_uniqueId (Some(Sc_bool b), var.s_var_name)
    | Sc_int i,  St_int  -> Hashtbl.replace env.var var.s_var_uniqueId (Some (Sc_int i), var.s_var_name)
    | _, _ -> raise (Interpretation_exception "Invalid type : variable incompatible with expression")

(* 
 * Interpret conditions
 *)
let rec interpret_condition env cond blk1 blk2 =
    match interpret_expr env cond with
    | Sc_bool b when b -> interpret_block env blk1
    | Sc_bool _        -> interpret_block env blk2
    | _                -> raise (Interpretation_exception "Invalid type : St_bool expected")

(*
 * Interpret loops
 *)
and interpret_loop env cond blk = 
    match interpret_expr env cond with
    | Sc_bool b when b -> interpret_block env blk; interpret_loop env cond blk
    | Sc_bool _        -> ();
    | _                -> raise (Interpretation_exception "Invalid type : St_bool expected")

(*
 * Interpret procedure call
 *)
and interpret_proc env proc = 
    let p = (try Hashtbl.find env.proc (proc.s_proc_call_class,proc.s_proc_call_name)
    with Not_found -> raise (Interpretation_exception "Undefined procedure"))
    in interpret_block env p

(*
 * Interpret assert
 *)
and interpret_assert env expr =
    match interpret_expr env expr with
    | Sc_bool b when b -> ()
    | Sc_bool _        -> raise (Interpretation_exception "Assert failure")
    | _                -> raise (Interpretation_exception "Invalid type : St_bool expected")

(*
 * Interpret instructions
 *)
and interpret_command env cmd =
    match cmd with
    | Sc_assign (var, expr)    -> interpret_assign env var expr
    | Sc_if (cond, blk1, blk2) -> interpret_condition env cond blk1 blk2
    | Sc_while (cond, blk)     -> interpret_loop env cond blk
    | Sc_proc_call proc        -> interpret_proc env proc
    | Sc_assert expr           -> interpret_assert env expr

(*
 * Interpret block of instructions
 *)
and interpret_block env blk =
    match blk with
    | []            -> ()
    | (cmd, loc)::q -> (try interpret_command env cmd
    with Interpretation_exception e -> fail_message e loc);
    interpret_block env q

(*
 * List and initialize variable declaration
 *)
let interpret_var_decl env (var,init) =
    match init with
    | None   -> Hashtbl.replace env.var var.s_var_uniqueId (None, var.s_var_name)
    | Some e -> match interpret_expr env e with
        | Sc_int i when var.s_var_type = St_int  -> Hashtbl.replace env.var var.s_var_uniqueId (Some (Sc_int i), var.s_var_name)
        | Sc_bool b when var.s_var_type = St_bool -> Hashtbl.replace env.var var.s_var_uniqueId (Some (Sc_bool b), var.s_var_name)
        | _ -> raise (Interpretation_exception (Printf.sprintf "Invalid init type : variable %s incompatible with expression type" var.s_var_name))

(*
 * List and store functions
 *)
let interpret_proc_decl env className p = 
    Hashtbl.replace env.proc (className, p.s_proc_name) p.s_proc_body

(*
 * Interpret class definitions
 *)
let interpret_class env c = 
    let rec readClassDeclaration l = match l with
    | []    -> ()
    | h::q  -> (match h with
        | Sd_var v      -> interpret_var_decl env v
        | Sd_function p -> interpret_proc_decl env c.s_class_name p
    );
    readClassDeclaration q
    in readClassDeclaration c.s_class_body


exception Found of s_block

(*
 * Interpret a program
 *)
let interpret_program (p:s_program) : unit =
    (* Initilize random generator *)
    Random.self_init();
    let env = {
        var  = Hashtbl.create 10;
        proc = Hashtbl.create 10} 
    in
    (* Read all declration in the program *)
    let rec readDeclarations l = match l with
    | []   -> ()
    | h::q -> interpret_class env h; readDeclarations q
    in readDeclarations p;

    (* print variables at the begining of the exection *)
    print_endline "\nBegin :\n-----------";
    print_variables env p;
    print_endline "------------\n";

    (* Look for function main and run it *)
    try
        Hashtbl.iter (fun (c, f) body -> if f = "main" then raise (Found body)) env.proc
    with Found body -> interpret_block env body;
    (* Print variables at the end of execution *)
    print_endline "\nEnd :\n-----------";
    print_variables env p;
    print_endline "------------"





