
module type AbstractField = sig
    type t
    val undef : t
    val unreach : t
    
    (* Return the smallest abstract element greater than both parameters *)
    val merge : t -> t -> t
    val intersect : t -> t -> t
    val enlarge : t -> t

    (* Convert a concrete value to an abstract one *)
    val convertVal : Simple_java_syntax.s_constant -> t
    val convertInterval : int64 ->  int64 -> t
    val isVal : int64 -> t -> bool
    val isValIn : int64 -> t -> bool
    val equal : t -> t -> bool

    val toString : t -> string

    (* Binary operations on abstract type *)
    val binOp : Simple_java_syntax.s_binary_op -> t -> t -> t
    (* Unary operations on abstract type *)
    val unOp  : Simple_java_syntax.s_unary_op -> t -> t

end
