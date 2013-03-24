let main () =
    let self = FileUtil.readlink Sys.executable_name in
    let baseDir = String.sub self 0 (String.rindex self '/') in
    Interpreter.Main.addExtension (baseDir ^ "/extensions/core/misc.cmo");
    Interpreter.Main.addExtension (baseDir ^ "/extensions/core/closure.cmo");
    Interpreter.Main.addExtension (baseDir ^ "/extensions/core/exception.cmo");
    let filename =
        if Array.length Sys.argv = 2 then
            Sys.argv.(1)
        else
            ""
    in
    Interpreter.Main.run filename
      
let _ = main ()

