open Language.Typing

let dirname context args =
    let `String path = to_string (List.nth args 0)#get in
    new variable (`String (String.sub path 0 (String.rindex path '/')))

let is_file context args =
    let `String path = to_string (List.nth args 0)#get in
    new variable (`Bool
        (try
            (Unix.stat path).Unix.st_kind = Unix.S_REG
        with Unix.Unix_error _ -> false))

let is_dir context args =
    let `String path = to_string (List.nth args 0)#get in
    new variable (`Bool
        (try
            (Unix.stat path).Unix.st_kind = Unix.S_DIR
        with Unix.Unix_error _ -> false))


let _ = Interpreter.Extension.register
    "core/files"
    [
        ("DIRECTORY_SEPARATOR", `String "/");
        ("PATH_SEPARATOR", `String ":")
    ]
    [
        ("dirname", dirname);
        ("is_file", is_file);
        ("is_dir", is_dir)
    ]
    []
    []

