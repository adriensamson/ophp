open Language.Typing

let error_reporting context args =
    new variable (`Long 32767)

let _ = Interpreter.Extension.register
    "core/error"
    []
    [
        ("error_reporting", error_reporting)
    ]
    []
    []

