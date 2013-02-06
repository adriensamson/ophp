let run chan = 
    let context = Expression.makeContext (new Variable.variableRegistry) in
    let f l =
        match Statement.exec_list context l with
            | Statement.Return v -> v
            | _ -> `Bool true
    in
    Registry.files#setExec f;
    let _ = Registry.files#runChannel chan in
    ()
