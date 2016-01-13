open Simple_java_syntax

(*
 * Interpret a simple Java programm from Syntax tree
 *)

(*
 * - chercher le main
 * - 
 *)

type env = {
    var : (s_var, s_constant option) Hashtbl.t;
    proc : ((string*string), s_block) Hashtbl.t
}

let check_type t a =
    match a with
    | Sc_int _ -> if t <> St_int then failwith "Invalid type : St_int expected"
    | Sc_bool _ -> if t <> St_bool then failwith "Invalid type : St_bool expected"

let rec interpret_unary env op e = 
    match op with
    | Su_neg -> let v = interpret_expr env e in
    (match v with 
        | Sc_bool b -> Sc_bool (not b)
        | _ -> failwith "Invalid type : St_bool expected")

    and interpret_binary env op a b = failwith "TODO"


    and interpret_expr env (expr, ext) = 
    match expr with
    | Se_const e -> e
    | Se_random (a,b) -> Sc_int(Int64.add a (Random.int64 (Int64.sub b a)))
    | Se_var var -> failwith "TODO"
    | Se_unary (op, e) -> interpret_unary env op e
    | Se_binary (op, a, b) -> interpret_binary env op a b

let interpret_var_decl env v =
    let var, init = v in
    match init with
    | None -> Hashtbl.replace env.var var None 
    | Some e -> Hashtbl.replace env.var var (Some (interpret_expr env e))

let interpret_proc env className p = 
    Hashtbl.replace env.proc (className, p.s_proc_name) p.s_proc_body

let interpret_class env c = 
    let rec readClassDeclaration l = match l with
    | [] -> ()
    | h::q -> (match h with
        | Sd_var v -> interpret_var_decl env v
        | Sd_function p -> interpret_proc env c.s_class_name p
    )
    in readClassDeclaration c.s_class_body

let interpret_program (p:s_program) : unit =
    let env = {
        var = Hashtbl.create 10;
        proc = Hashtbl.create 10} 
    in
    let rec readDeclarations l = match l with
    | [] -> ()
    | h::q -> interpret_class env h; readDeclarations q
    in readDeclarations p





