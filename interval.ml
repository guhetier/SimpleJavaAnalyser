
open AbstractField

module Interval : AbstractField = struct

    type t = Interval of int64 * int64 | Top | Unreach | Undef
    
    let undef = Undef
    let unreach = Unreach

    let merge a b =
        match a, b with
        | Undef, _
        | _, Undef                           -> Undef
        | Unreach, x
        | x, Unreach                         -> x
        | Interval(ax, ay), Interval(bx, by) -> Interval(min ax bx, max ay by)
        | _                                  -> Top

    let intersect = merge

    let enlarge a =
        match a with
        | Interval _ -> Top
        | _ -> a

    let equal a b =
        match a,b with
        | Top, Top
        | Undef, Undef
        | Unreach, Unreach -> true
        | Interval(xa, xb), Interval(ya, yb) when xa = ya && xb = yb -> true
        | _ -> false
        
    let convertVal c =
        match c with
        | Simple_java_syntax.Sc_int i         -> Interval(i,i)
        | Simple_java_syntax.Sc_bool b when b -> Interval(1L,1L)
        | Simple_java_syntax.Sc_bool _        -> Interval(0L, 0L)

    let convertInterval binf bsup =
        Interval(binf, bsup)

    let isVal i v =
        match v with
        | Interval(a,b) when a = i && b = i -> true
        | _ -> false

    let isValIn i v =
        match v with
        | Top -> true
        | Interval(a,b) when i >= a && i <= b  -> true
        | _ -> false

    let toString v =
        match v with
        | Top     -> "Top"
        | Undef   -> "Non defined"
        | Unreach -> "Non reachable"
        | Interval(a, b) -> Printf.sprintf "[%s ; %s]" (Int64.to_string a) (Int64.to_string b)

    let applyNeg i =
        match i with
        | Top     -> Top
        | Undef   -> Undef
        | Unreach -> Unreach
        | Interval(a, b) when a = 0L && b = 0L -> Interval(1L, 1L)
        | Interval(a, b) when not (isValIn 0L i) -> Interval(0L, 0L)
        | _ -> Top

    let applyBinOp op a b =
        match a, b with
        | Undef, _ | _, Undef     -> Undef
        | Top, _ | _, Top         -> Top
        | Unreach, _ | _, Unreach -> Unreach
        | Interval(ax, ay), Interval(bx, by) -> Interval(op ax bx, op ay by)

    let applyProd op a b =
        match a, b with
        | Undef, _ | _, Undef     -> Undef
        | Top, _ | _, Top         -> Top
        | Unreach, _ | _, Unreach -> Unreach
        | Interval(ax, ay), Interval(bx, by) -> Interval(min (min (op ax bx) (op ay by)) (min (op ax by) (op ay bx)), max (max (op ax bx) (op ay by)) (max (op ax by) (op ay bx)))

    let applyComp a b =
        match a, b with
        | Undef, _ | _, Undef     -> Undef
        | Top, _ | _, Top         -> Top
        | Unreach, _ | _, Unreach -> Unreach
        | Interval(ax, ay), Interval(bx, by) when ay < bx -> Interval(1L, 1L) 
        | Interval(ax, ay), Interval(bx, by) when by <= ax -> Interval(0L, 0L) 
        | _ -> Top

    let binOp op a b =
        match op with
        | Simple_java_syntax.Sb_add -> applyBinOp Int64.add a b
        | Simple_java_syntax.Sb_sub -> applyBinOp Int64.sub a b
        | Simple_java_syntax.Sb_mul -> applyProd Int64.mul a b
        | Simple_java_syntax.Sb_div -> applyProd Int64.div a b
        | Simple_java_syntax.Sb_lt  -> applyComp a b
        | Simple_java_syntax.Sb_or  -> applyBinOp (fun a b -> if (Int64.add a b) > 1L then 1L else 0L) a b

    let unOp op a =
        match op with
        | Simple_java_syntax.Su_neg -> applyNeg a

end
