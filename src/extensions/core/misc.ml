open Language.Typing

let define c l =
    let `String name = Language.Typing.to_string (List.nth l 0)#get in
    c#constants#add name (List.nth l 1)#get;
    new variable (`Bool true)

let defined c l =
    let `String name = Language.Typing.to_string (List.nth l 0)#get in
    new variable (`Bool (c#constants#has name))

let _ = Interpreter.Extension.register
    "core/misc"
    []
    [
        ("define", define);
        ("defined", defined)
    ]
    []
    []

