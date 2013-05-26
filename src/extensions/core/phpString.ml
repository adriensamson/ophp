open Language.Typing

let strpos_real haystack needle =
    let result = ref (-1) in
    let current_offset = ref 0 in
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

let strrpos_real haystack needle =
    let result = ref (-1) in
    let hl = String.length haystack in
    let current_offset = ref hl in
    let nl = String.length needle in
    try
        while !current_offset > 0 && !result = -1 do
            let index = String.rindex_from haystack !current_offset needle.[0] in
            if String.sub haystack index nl = needle then
                result := index
            else
                current_offset := index - 1
        done;
        !result
    with
    | Not_found -> -1

let ltrim_real s chars =
    let i = ref 0 in
    let l = String.length s in
    while !i < l && String.contains chars s.[!i] do
        incr i
    done;
    String.sub s !i (l - !i)
let rtrim_real s chars =
    let l = String.length s in
    let i = ref (l - 1) in
    while !i > 0 && String.contains chars s.[!i] do
        decr i
    done;
    String.sub s 0 !i



let strlen context args =
    let `String s = to_string (List.nth args 0)#get in
    new variable (`Long (String.length s))

let strpos context args =
    let `String haystack = to_string (List.nth args 0)#get in
    let needle = match (List.nth args 1)#get with
        | `String s -> s
        | v -> let `Long i = to_long v in String.make 1 (char_of_int i)
    in
    let offset = if List.length args > 2 then let `Long i = to_long (List.nth args 2)#get in i else 0 in
    let subs = match offset with
        | 0 -> haystack
        | i when i > 0 -> String.sub haystack i (String.length haystack - i)
        | i -> String.sub haystack (String.length haystack + i) (-i)
    in
    match strpos_real subs needle with
    | -1 -> new variable (`Bool false)
    | n -> new variable (`Long n)

let strrpos context args =
    let `String haystack = to_string (List.nth args 0)#get in
    let needle = match (List.nth args 1)#get with
        | `String s -> s
        | v -> let `Long i = to_long v in String.make 1 (char_of_int i)
    in
    let offset = if List.length args > 2 then let `Long i = to_long (List.nth args 2)#get in i else 0 in
    let subs = match offset with
        | 0 -> haystack
        | i when i > 0 -> String.sub haystack i (String.length haystack - i)
        | i -> String.sub haystack (String.length haystack + i) (-i)
    in
    match strpos_real subs needle with
    | -1 -> new variable (`Bool false)
    | n -> new variable (`Long n)


let trim context args =
    let `String s = to_string (List.nth args 0)#get in
    let `String chars = try to_string (List.nth args 0)#get with Not_found -> `String " \t\n\r\x00\x0B" in (* TODO expand .. *)
    new variable (`String (ltrim_real (rtrim_real s chars) chars))

let _ = Interpreter.Extension.register
    "core/string"
    []
    [
        ("strlen", strlen);
        ("strpos", strpos);
        ("strrpos", strrpos);
        ("trim", trim)
    ]
    []
    []

