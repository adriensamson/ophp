open Language.Typing

let rec split_string s off l =
    try
        let i = String.rindex_from s off ':' in
        split_string s (i-1) ((String.sub s (i+1) (off-i))::l)
    with
    | Not_found ->
        if off > 0 then
            (String.sub s 0 (off+1))::l
        else l

let set_include_path c l =
    let `String paths = to_string (List.nth l 0)#get in
    let pathl = split_string paths (String.length paths - 1) [] in
    c#files#setIncludePaths pathl;
    new variable (`Bool true)

let _ = Interpreter.Extension.register
    "core/info"
    []
    [
        ("get_include_path", fun c l -> new variable (`String (String.concat ":" c#files#includePaths)));
        ("set_include_path", set_include_path)
    ]
    []
    []

