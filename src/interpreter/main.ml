let run stmt_list = 
    let variables = new Variable.variableRegistry in
    let _ = Statement.exec_list variables stmt_list in
    ()
