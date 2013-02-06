let run chan = 
    let context = Expression.makeContext (new Variable.variableRegistry) in
    let f l =
        let _ = Statement.exec_list context l in
        `Long 1
    in
    Registry.files#setExec f;
    let _ = Registry.files#runChannel chan in
    ()
