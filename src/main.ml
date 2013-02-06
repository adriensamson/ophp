let main () =
    let chan = stdin in
    Interpreter.Main.run chan
      
let _ = main ()

