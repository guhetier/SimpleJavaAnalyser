module VarOrd : (Map.OrderedType with type t = Simple_java_syntax.s_var) = struct
    type t = Simple_java_syntax.s_var
    
    let compare v1 v2 =
        Pervasives.compare v1.Simple_java_syntax.s_var_uniqueId v2.Simple_java_syntax.s_var_uniqueId

end

module VarMap = Map.Make(VarOrd)
