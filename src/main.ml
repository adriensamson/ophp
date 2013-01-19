let main () =
    let lexbuf = Lexing.from_channel stdin in
    let stmt_list = Parser.everything Lexer.parse lexbuf in
    Interpreter.Main.run stmt_list
      
let _ = main ()

