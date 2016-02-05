open AbstractField

module Constant : AbstractField = struct

    type t = Const of int64 | Top | Undef | Unreach
    
    let undef = Undef
    let unreach = Unreach

    let merge a b =
        match a, b with
        | Undef, _
        | _, Undef                        -> Undef
        | Unreach, x
        | x, Unreach                      -> x
        | Const aa, Const bb when aa = bb -> a
        | _                               -> Top

    let intersect = merge

    let equal a b =
        match a,b with
        | Top, Top
        | Undef, Undef
        | Unreach, Unreach -> true
        | Const(a), Const(b) when a = b -> true
        | _ -> false
        
    let convertVal c =
        match c with
        | Simple_java_syntax.Sc_int i         -> Const i
        | Simple_java_syntax.Sc_bool b when b -> Const 1L
        | Simple_java_syntax.Sc_bool _        -> Const 0L

    let convertInterval binf bsup =
        Top

    let isVal i v =
        match v with
        | Const v when v = i -> true
        | _ -> false

    let isValIn i v =
        match v with
        | Top -> true
        | Const v when v = i -> true
        | _ -> false

    let isValOut i v =
        match v with
        | Top -> false
        | Const v when v = i -> false
        | _ -> true

    let toString v =
        match v with
        | Top     -> "Top"
        | Undef   -> "Non defined"
        | Unreach -> "Non reachable"
        | Const a -> Printf.sprintf "Const(%s)" (Int64.to_string a)

    let applyUnOp op a =
        match a with
        | Top     -> Top
        | Undef   -> Undef
        | Unreach -> Unreach
        | Const a -> Const(op a)

    let applyBinOp op a b =
        match a, b with
        | Undef, _ | _, Undef     -> Undef
        | Top, _ | _, Top         -> Top
        | Unreach, _ | _, Unreach -> Unreach
        | Const a, Const b        -> Const(op a b)

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
