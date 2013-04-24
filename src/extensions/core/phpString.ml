open Language.Typing

let strpos_real haystack needle offset =
    let result = ref (-1) in
    let current_offset = ref offset in
    let hl = String.length haystack in
    let nl = String.length needle in
    try
        while !current_offset < hl && !result = -1 do
            let index = String.index_from haystack !current_offset needle.[0] in
            if String.sub haystack index nl = needle then
                result := index
            else
                current_offset := index + 1
        done;
        !result
    with
    | Not_found -> -1

let strpos context args =
    let `String haystack = to_string (List.nth args 0)#get in
    let needle = match (List.nth args 1)#get with
        | `String s -> s
        | v -> let `Long i = to_long v in String.make 1 (char_of_int i)
    in
    let offset = if List.length args > 2 then let `Long i = to_long (List.nth args 2)#get in i else 0 in
    match strpos_real haystack needle offset with
    | -1 -> new variable (`Bool false)
    | n -> new variable (`Long n)

let _ = Interpreter.Extension.register
    "core/string"
    []
    [
        ("strpos", strpos)
    ]
    []
    []

