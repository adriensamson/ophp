let run chan = 
    let context = Expression.makeContext (new Variable.variableRegistry) in
    let fr = new Registry.functionRegistry in
    let cr = new Registry.classRegistry in
    let parse chan =
        Syntax.Lexer.reset ();
        let lexbuf = Lexing.from_channel chan in
        Syntax.Parser.everything Syntax.Lexer.parse lexbuf
    in
    let filesr = new Registry.fileRegistry parse in
    let evaluator = new Expression.evaluator fr cr filesr in
    let executor = new Statement.executor fr cr evaluator in
    let _ = executor#exec_file context (filesr#parseChannel chan) in
    ()
