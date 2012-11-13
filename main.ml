let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let stmt = Parser.stmt Lexer.token lexbuf in
      Ast.exec stmt
    done
  with End_of_file -> exit 0
      
let _ = main ()

