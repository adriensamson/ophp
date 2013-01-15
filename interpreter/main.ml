let run stmt_list = 
    let variables = Hashtbl.create 10 in
    let _ = Statement.exec_list variables stmt_list in
    ()
