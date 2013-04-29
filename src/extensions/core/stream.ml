open Language.Typing

let stream_resolve_include_path c l =
    let `String f = to_string (List.nth l 0)#get in
    new variable (try `String (c#files#resolveFilename f) with _ -> `Bool false)

let _ = Interpreter.Extension.register
    "core/stream"
    []
    [
        ("stream_resolve_include_path", stream_resolve_include_path)
    ]
    []
    []

