open Core
open Spl
open Compat

let main () =
    let self = FileUtil.readlink Sys.executable_name in
    let baseDir = String.sub self 0 (String.rindex self '/') in
    List.iter
        (fun e -> Interpreter.Main.addExtension (baseDir ^ "/extensions/" ^ e ^ ".cmo"))
        ["core"; "spl"; "compat"; "dom"];
    let filename =
        if Array.length Sys.argv = 2 then
            Sys.argv.(1)
        else
            ""
    in
    Interpreter.Main.run filename
      
let _ = main ()

