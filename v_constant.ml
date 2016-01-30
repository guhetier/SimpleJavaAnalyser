
open Simple_java_syntax

(*
 * Interpret a simple Java programm from Syntax tree
 *)

(*
 * Program environement
 * var : s_var <-> actual value of the variable
 * proc : className, procName <-> proc instruction
 *)

type value = Det of s_constant | Undet | Unreach

type env = {
    var : (s_uniqueId, s_var * value) Hashtbl.t;
    proc : ((string*string), s_block) Hashtbl.t
}

type locState = (Localizing.extent, env) Hashtbl.t

let getLocValue env var ext =
    try
    Hashtbl.find (Hashtbl.find env var.s_var_uniqueId) ext
    with Not_found -> Undet

let setLocValue env var ext value =
    Hashtbl.replace env var.s_var_uniqueId

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
    let print_var id (var, value) =
        Printf.printf "%s -> %s\n"
        var.s_var_name
        (match value with
        | Det(Sc_bool b) -> Printf.sprintf "Det(%s)" (if b then "True" else "False")
        | Det(Sc_int i)  -> Printf.sprintf "Det(%i)" (Int64.to_int i)
        | Undet          -> "Undetermined"
        | Unreach        -> "Unreachable"
        )
    in
    Hashtbl.iter print_var env.var

(*
 * Interpret variable access
 *)
let interpret_var env var = 
    try snd(Hashtbl.find env.var var.s_var_uniqueId)
    with Not_found -> Undet
    
(*
 * Interpret unary operators
 *)
let rec interpret_unary env op e = 
    match op with
    | Su_neg -> match interpret_expr env e  with 
        | Det(Sc_bool b) -> Det(Sc_bool (not b))
        | _ -> Undet
        
(*
 * Interpret binary operators
 *)
    and interpret_binary env op a b =
    let compute_op op va vb =
        match va, vb with
        | Det(Sc_int ia), Det(Sc_int ib) -> (match op with
            | Sb_add -> Det(Sc_int(Int64.add ia ib))
            | Sb_sub -> Det(Sc_int(Int64.sub ia ib))
            | Sb_mul -> Det(Sc_int(Int64.mul ia ib))
            | Sb_div -> if ib = Int64.zero
                        then raise (Interpretation_exception "Invalid operation : division by zero");
                        Det(Sc_int(Int64.div ia ib))
            | Sb_lt  -> Det(Sc_bool(ia < ib))
            | _ -> failwith "Should not happen"
            )
        | _, _ -> Undet
    in
    match op with
        | Sb_or                  -> (match interpret_expr env a, interpret_expr env b with
            | Det(Sc_bool bb), _
            | _, Det(Sc_bool bb) when bb -> Det(Sc_bool true) 
            | Det(Sc_bool b1), Det(Sc_bool b2) when not(b1 || b2) -> Det(Sc_bool(false))
            | _ -> Undet
            )
        | _ -> compute_op op (interpret_expr env a) (interpret_expr env b)

(*
 * Interpret expressions
 *)
    and interpret_expr env (expr, ext) = 
        try (
        match expr with
        | Se_const e           -> Det(e)
        | Se_random (a,b)      -> Det(Sc_int(Int64.add a (Random.int64 (Int64.sub b a))))
        | Se_var var           -> interpret_var env var
        | Se_unary (op, e)     -> interpret_unary env op e
        | Se_binary (op, a, b) -> interpret_binary env op a b
        ) with Interpretation_exception e -> fail_message e ext

(*
 * Interpret variable assignment
 *)
let interpret_assign env var expr =
    Hashtbl.replace env.var var.s_var_uniqueId (var, (interpret_expr env expr))

(* 
 * Interpret conditions
 *)
let rec interpret_condition env cond blk1 blk2 =
    match interpret_expr env cond with
    | Det(Sc_bool b) when b -> interpret_block env blk1; failwith "TODO : other branch unreachable"
    | Det(Sc_bool _)        -> interpret_block env blk2; failwith "TODO : other branch unreachable"
    | _                     -> failwith "TODO"

(*
 * Interpret loops
 *)
and interpret_loop env cond blk = 
    match interpret_expr env cond with
    | _                -> failwith "TODO : loop until constant state. Each time a var change => undet"

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
    | Det(Sc_bool b) when not b -> failwith "TODO : après une assert raté, le programme s'interompt : tout ce qui suit est unreachable"
    | _                -> ()

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
    | None   -> Hashtbl.replace env.var var.s_var_uniqueId (var,Undet)
    | Some e -> Hashtbl.replace env.var var.s_var_uniqueId (var, interpret_expr env e)

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
    print_variables env;
    print_endline "------------\n";

    (* Look for function main and run it *)
    try
        Hashtbl.iter (fun (c, f) body -> if f = "main" then raise (Found body)) env.proc
    with Found body -> interpret_block env body;
    (* Print variables at the end of execution *)
    print_endline "\nEnd :\n-----------";
    print_variables env;
    print_endline "------------"





