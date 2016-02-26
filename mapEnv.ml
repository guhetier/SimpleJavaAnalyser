open Environment
open CustomMaps
open AbstractField
open Simple_java_syntax

module Make (Field: AbstractField) : Environment = struct

    module Field = Field

    type state = Field.t VarMap.t
    type records = state LocMap.t
    type t = {
        currentState : state;
        recordedStates : records
    }

    let empty = {
        currentState = VarMap.empty;
        recordedStates = LocMap.empty
    }
    
    let stateToString (state : state) =
        let ss = VarMap.fold
            (fun var value l ->
                (Printf.sprintf "{ %s : %s }" var.s_var_name (Field.toString value))::l)
            state []
        in String.concat ", " ss

    let recordToStrings env =
        LocMap.map stateToString env.recordedStates

    let getValue env var =
        try VarMap.find var env.currentState
        with Not_found -> Field.undef

    let setValue env var value =
        {
            currentState = (VarMap.add var value env.currentState);
            recordedStates = env.recordedStates
        }

    let unreachable env =
        {
            currentState = VarMap.map (fun _ -> Field.unreach) env.currentState;
            recordedStates = env.recordedStates
        }

    let isUnreachable env =
        VarMap.for_all (fun _ v -> Field.equal v Field.unreach) env.currentState

    let isStateUnreachable s =
        VarMap.for_all (fun _ v -> Field.equal v Field.unreach) s


    let setUnreachable env loc =
        {
            currentState = env.currentState;
            recordedStates = LocMap.add loc (VarMap.map (fun _ -> Field.unreach) env.currentState) env.recordedStates
        }

    let mergeState s1 s2 =
        VarMap.merge (fun _ v1 v2 ->
                        match v1, v2 with
                        | None, v
                        | v, None -> v
                        | Some v1, Some v2  -> Some(Field.merge v1 v2)
        ) s1 s2

    let mergeEnv env1 env2 =
        let recordMerge = LocMap.merge (fun _ s1 s2 ->
                            match s1, s2 with
                            | None, s
                            | s, None -> s                    
                            | Some s1, Some s2 -> Some (mergeState s1 s2)
        ) env1.recordedStates env2.recordedStates
        in
        {
            currentState = (mergeState env1.currentState env2.currentState);
            recordedStates = recordMerge
        }


    let recordState env ext =
        let prevState = try LocMap.find ext env.recordedStates
                        with Not_found -> VarMap.empty
        in {
            currentState = env.currentState;
            recordedStates = (LocMap.add ext (mergeState prevState env.currentState)
                                env.recordedStates)
        }

    let clearRecords env =
        {
            currentState = env.currentState;
            recordedStates = LocMap.empty
        }
        
    (*
     * If new code is reach, return None
     * Else, return the list a variable whose value changed in the loop
     * *)
    let varToEnlarge oldenv newenv =
        (* Check if new code have been reach *)
        
        let nothingNew = LocMap.for_all (fun k s2 ->
            if isStateUnreachable s2 then true
            else begin
                try
                let s1 = LocMap.find k oldenv.recordedStates
                in not (isStateUnreachable s1)
                with Not_found -> false
            end
        ) newenv.recordedStates
        in
        if not nothingNew then
            None
        else
            begin

                (* Look for variables which value changed *)
                let res = VarMap.fold (fun k v1 m2 ->
                    let v2 = try VarMap.find k m2 with Not_found -> Field.undef in
                    if Field.equal v1 v2 then
                        VarMap.remove k m2
                    else
                        m2  
                    ) oldenv.currentState newenv.currentState
                in
                Some(List.map fst (VarMap.bindings res))
            end
        
    let enlarge env l =
        List.fold_left (fun env var ->
                   setValue env var (Field.enlarge (getValue env var))
        ) env l

end


