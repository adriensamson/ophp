let run stmt_list = 
    let context = {Expression.vars = new Variable.variableRegistry; obj = None; callingClass = None} in
    let _ = Statement.exec_list context stmt_list in
    ()
