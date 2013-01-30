let run stmt_list = 
    let context = Expression.makeContext (new Variable.variableRegistry) in
    let _ = Statement.exec_list context stmt_list in
    ()
