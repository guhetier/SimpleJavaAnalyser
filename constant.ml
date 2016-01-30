open Simple_java_syntax
open AbstractField

module Constant : AbstractField = struct

    type t = Const of int64 | Top | Bottom
    type field = (Simple_java_syntax.s_var, t) Hashtbl.t
    
    let bottom = Bottom

    let setVar field var v =
        Hashtbl.replace field  var v

    let getVar field var =
        try Hashtbl.find field var
        with Not_found -> Bottom

    let convert c =
        match c with
        | Sc_int i -> Const i
        | Sc_bool b when b -> Const 1L
        | Sc_bool _ -> Const 0L

    let combine a b =
        match a, b with
        | Bottom, x
        | x, Bottom -> x
        | Const aa, Const bb when aa = bb -> a
        | _ -> Top

    let combineVar field var b =
        Hashtbl.replace field var (combine (getVar field var) b);
        true (*TODO : return boolan depending wether value changed *)

    let copy field =
        Hashtbl.copy field


    let binOp op a b =
        match op with
        | Simple_java_syntax.Sb_add
        | Simple_java_syntax.Sb_sub
        | Simple_java_syntax.Sb_mul
        | Simple_java_syntax.Sb_div
        | Simple_java_syntax.Sb_lt
        | Simple_java_syntax.Sb_or
        -> failwith "TODO"

    let unOp op a =
        failwith "TODO"

    let toString v =
        failwith "TODO"
    
end
