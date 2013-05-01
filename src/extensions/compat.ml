open Language.Typing
open Interpreter

let _ = Extension.register
    "compat"
    [("FALSE", `Bool false); ("TRUE", `Bool true); ("NULL", `Null)]
    []
    []
    []

