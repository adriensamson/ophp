let main () =
    let lexbuf = Lexing.from_channel stdin in
    let stmt_list = Parser.everything Lexer.token lexbuf in
    let variables = Hashtbl.create 10 in
    List.iter (Ast.exec variables) stmt_list
      
let _ = main ()

