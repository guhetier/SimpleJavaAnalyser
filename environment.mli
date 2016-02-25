open AbstractField
open CustomMaps

module type Environment = sig

    module Field : AbstractField

    type t

    type state = Field.t VarMap.t

    type records
    
    val empty : t
    val stateToString : state -> string
    
    val getValue : t -> Simple_java_syntax.s_var -> Field.t
    val setValue : t -> Simple_java_syntax.s_var -> Field.t -> t
    val unreachable : t -> t
    val isUnreachable : t -> bool

    val mergeEnv : t -> t -> t
    val recordState : t -> Localizing.extent -> t
    val varToEnlarge : t -> t -> Simple_java_syntax.s_var list option
    val enlarge : t -> Simple_java_syntax.s_var list -> t

 


end
