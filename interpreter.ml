open Simple_java_syntax

(*
 * Interpret a simple Java programm from Syntax tree
 *)

(*
 * - chercher le main
 * - 
 *)

(*
 * Program environement
 * var : s_var <-> actual value of the variable
 * proc : className, procName <-> proc instruction
 *)
type env = {
    var : (s_var, s_constant option) Hashtbl.t;
    proc : ((string*string), s_block) Hashtbl.t
}

let check_type t a =
    match a with
    | Sc_int _ -> if t <> St_int then failwith "Invalid type : St_int expected"
    | Sc_bool _ -> if t <> St_bool then failwith "Invalid type : St_bool expected"

(*
 * Interpret unary operators
 *)
let rec interpret_unary env op e = 
    match op with
    | Su_neg -> let v = interpret_expr env e in
    (match v with 
        | Sc_bool b -> Sc_bool (not b)
        | _ -> failwith "Invalid type : St_bool expected"
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
                        then failwith "Invalid operation : division by zero";
                        Sc_int(Int64.div ia ib)
            | Sb_lt  -> Sc_bool(ia < ib)
            | _ -> failwith "Should not happen"
            )
        | _, _ -> failwith "Invalid type : St_int*St_int expected"
    in
    match op with
        | Sb_or -> (match interpret_expr env a with
            | Sc_bool b1 when b1 -> Sc_bool true
            | Sc_bool b1 -> (match interpret_expr env b with
                | Sc_bool b2 -> Sc_bool b2
                | _ -> failwith "Invalid type : St_bool*St_bool expected"
                )
            | _ -> failwith "Invalid type : St_bool*St_bool expected"
            )
        | _ -> compute_op op (interpret_expr env a) (interpret_expr env b)

(*
 * Interpret expressions
 *)
    and interpret_expr env (expr, ext) = 
        match expr with
        | Se_const e -> e
        | Se_random (a,b) -> Sc_int(Int64.add a (Random.int64 (Int64.sub b a)))
        | Se_var var -> (match (try Hashtbl.find env.var var
                        with Not_found -> failwith "Undeclared variable") with
                        | Some v -> v
                        | None -> failwith "Non initialized variable"
                        )
        | Se_unary (op, e) -> interpret_unary env op e
        | Se_binary (op, a, b) -> interpret_binary env op a b


(* 
 * Interpret conditions
*)
let rec interpret_condition env cond blk1 blk2 =
    match interpret_expr env cond with
    | Sc_bool b when b -> interpret_block env blk1
    | Sc_bool _ -> interpret_block env blk2
    | _ -> failwith "Invalid type : St_bool expected"

(*
 * Interpret loops
 *)
and interpret_loop env cond blk = failwith "Todo"


(*
 * Interpret assert
 *)
and interpret_assert env expr =
    match interpret_expr env expr with
    | Sc_bool b when b -> ()
    | Sc_bool _ -> failwith "Assert failure"
    | _ -> failwith "Invalid type : St_bool expected"

(*
 * Interpret instructions
 *)
and interpret_command env cmd =
    match cmd with
    | Sc_assign (var, expr) -> failwith "Todo"
    | Sc_if (cond, blk1, blk2) -> interpret_condition env cond blk1 blk2
    | Sc_while (cond, blk) -> interpret_loop env cond blk
    | Sc_proc_call proc_call -> failwith "TODO"
    | Sc_assert expr -> interpret_assert env expr

and interpret_block env blk =
    match blk with
    | [] -> ()
    | (cmd, loc)::q -> interpret_command env cmd; interpret_block env q

(*
 * List and initialize variable declaration
 *)
let interpret_var_decl env v =
    let var, init = v in
    match init with
    | None -> Hashtbl.replace env.var var None 
    | Some e -> Hashtbl.replace env.var var (Some (interpret_expr env e))

(*
 * List and store function
 *)
let interpret_proc env className p = 
    Hashtbl.replace env.proc (className, p.s_proc_name) p.s_proc_body

(*
 * Interpret class definitions
 *)
let interpret_class env c = 
    let rec readClassDeclaration l = match l with
    | [] -> ()
    | h::q -> (match h with
        | Sd_var v -> interpret_var_decl env v
        | Sd_function p -> interpret_proc env c.s_class_name p
    )
    in readClassDeclaration c.s_class_body

(*
 * Interpret a program
 *)
let interpret_program (p:s_program) : unit =
    let env = {
        var = Hashtbl.create 10;
        proc = Hashtbl.create 10} 
    in
    let rec readDeclarations l = match l with
    | [] -> ()
    | h::q -> interpret_class env h; readDeclarations q
    in readDeclarations p





