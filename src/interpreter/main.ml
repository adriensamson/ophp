let run chan = 
    let context = Expression.makeContext (new Variable.variableRegistry) in
    let fr = new Registry.functionRegistry in
    let cr = new Registry.classRegistry in
    let filesr = new Registry.fileRegistry in 
    let evaluator = new Expression.evaluator fr cr filesr in
    let executor = new Statement.executor fr cr evaluator#eval in
    let f l =
        match executor#exec_list context l with
            | Statement.Return v -> v
            | _ -> `Bool true
    in
    filesr#setExec f;
    let _ = filesr#runChannel chan in
    ()
