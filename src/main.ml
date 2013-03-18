let main () =
    let chan = stdin in
    let self = Unix.readlink "/proc/self/exe" in
    let baseDir = String.sub self 0 (String.rindex self '/') in
    Interpreter.Main.addExtension (baseDir ^ "/extensions/core/closure.cmo");
    Interpreter.Main.run chan
      
let _ = main ()

