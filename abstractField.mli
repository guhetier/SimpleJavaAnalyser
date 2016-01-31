
module type AbstractField = sig
    type t
    type field
    val bottom : t

    (* Binary operations on abstract type *)
    val binOp : Simple_java_syntax.s_binary_op -> t -> t -> t
    (* Unary operations on abstract type *)
    val unOp  : Simple_java_syntax.s_unary_op -> t -> t

    (* Return the smallest abstract element greater than both parameters *)
    val combine : t -> t -> t
    (* Convert a concrete value to an abstract one *)
    val convertVal : Simple_java_syntax.s_constant -> t
    val convertInterval : int64 ->  int64 -> t
    val isVal : int64 -> t -> bool
    val isValIn : int64 -> t -> bool
    val isValOut : int64 -> t -> bool

    val getVar : field -> Simple_java_syntax.s_var -> t
    val setVar  : field -> Simple_java_syntax.s_var -> t -> unit
    val makeField : unit -> field
    val addNonInitVar : field -> Simple_java_syntax.s_var -> unit
    val combineVar : field -> Simple_java_syntax.s_var -> t -> bool

    val toString : t -> string
    val copy : field -> field



end
