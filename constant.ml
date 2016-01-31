open Simple_java_syntax
open AbstractField

module Constant : AbstractField = struct

    type t = Const of int64 | Top | Bottom
    type field = (Simple_java_syntax.s_var, t) Hashtbl.t
    
    let bottom = Bottom

    let makeField () =
        Hashtbl.create 10

    let addNonInitVar field var =
        Hashtbl.replace field var Bottom

    let setVar field var v =
        Hashtbl.replace field  var v

    let getVar field var =
        try Hashtbl.find field var
        with Not_found -> Bottom

    let convertVal c =
        match c with
        | Sc_int i         -> Const i
        | Sc_bool b when b -> Const 1L
        | Sc_bool _        -> Const 0L

    let convertInterval binf bsup =
        Top

    let combine a b =
        match a, b with
        | Bottom, x
        | x, Bottom                       -> x
        | Const aa, Const bb when aa = bb -> a
        | _                               -> Top

    let combineVar field var b =
        let oldval = getVar field var in
        let newval = combine oldval b in
        if oldval = newval then false
        else
        begin
            Hashtbl.replace field var (combine (getVar field var) b);
            true
        end

    let copy field =
        Hashtbl.copy field

    let toString v =
        match v with
        | Top     -> "Top"
        | Bottom  -> "Bottom"
        | Const a -> Printf.sprintf "Const(%s)" (Int64.to_string a)

    let applyUnOp op a =
        match a with
        | Top     -> Top
        | Bottom  -> Bottom
        | Const a -> Const(op a)

    let applyBinOp op a b =
        match a, b with
        | Top, _ | _, Top       -> Top
        | Bottom, _ | _, Bottom -> Bottom
        | Const a, Const b      -> Const(op a b)

    let binOp op a b =
        match op with
        | Simple_java_syntax.Sb_add -> applyBinOp Int64.add a b
        | Simple_java_syntax.Sb_sub -> applyBinOp Int64.sub a b
        | Simple_java_syntax.Sb_mul -> applyBinOp Int64.mul a b
        | Simple_java_syntax.Sb_div -> applyBinOp Int64.div a b
        | Simple_java_syntax.Sb_lt  -> applyBinOp (fun a b -> if a < b then 1L else 0L) a b
        | Simple_java_syntax.Sb_or  -> applyBinOp (fun a b -> if (Int64.add a b) > 1L then 1L else 0L) a b

    let unOp op a =
        match op with
        | Simple_java_syntax.Su_neg -> applyUnOp (fun a -> if a = 0L then 1L else 0L) a


end
