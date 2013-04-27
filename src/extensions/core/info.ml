open Language.Typing

let _ = Interpreter.Extension.register
    "core/info"
    []
    [
        ("get_include_path", fun c l -> new variable (`String "."))
    ]
    []
    []

