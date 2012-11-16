let main () =
    let lexbuf = Lexing.from_channel stdin in
    let stmt_list = Parser.everything Lexer.token lexbuf in
    Interpreter.run stmt_list
      
let _ = main ()

