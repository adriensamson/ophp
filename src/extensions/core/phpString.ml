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

let rec map2 f l1 l2 = match l1, l2 with
    | h1::t1, h2::t2 -> (f h1 (Some h2))::(map2 f t1 t2)
    | h1::t1, [] -> (f h1 None)::(map2 f t1 [])
    | [], _ -> []

let ltrim c args =
    let `String s = to_string (List.nth args 0)#get in
    let `String chars = try to_string (List.nth args 0)#get with Not_found -> `String " \t\n\r\x00\x0B" in (* TODO expand .. *)
    new variable (`String (ltrim_real s chars))

let rtrim c args =
    let `String s = to_string (List.nth args 0)#get in
    let `String chars = try to_string (List.nth args 0)#get with Not_found -> `String " \t\n\r\x00\x0B" in (* TODO expand .. *)
    new variable (`String (rtrim_real s chars))


let str_replace c args =
    let search = (List.nth args 0)#get
    and replace = (List.nth args 1)#get
    and subject = (List.nth args 2)#get
    (* and countVar = try List.nth args 3 with | Failure _ -> new variable (`Long 0) TODO *)
    in
    let pairs = match search, replace with
        | `Array a, `Array b -> map2
                (fun k1 k2 ->
                    let `String a = to_string (a#get k1)#get in
                    let b = match k2 with
                        | Some k2 -> let `String b = to_string (b#get k2)#get in b
                        | None -> ""
                    in a, b
                )
                a#keys
                b#keys
        | `Array a, b -> List.map (fun k -> let `String s1, `String s2 = to_string (a#get k)#get, to_string b in s1, s2) a#keys
        | _, `Array _ -> failwith "str_replace search must be an array when replace is an array"
        | _, _ -> let `String a, `String b = to_string search, to_string replace in [(a,b)]
    in
    let do_replace s =
        List.fold_left (fun s (from, dest) -> Str.global_replace (Str.regexp_string (Str.quote from)) dest s) s pairs
    in
    match subject with
    | `Array a ->
        let newArr = new Interpreter.PhpArray.phpArray in
        List.iter (fun k -> let `String s = to_string (a#get k)#get in newArr#set k (new variable (`String (do_replace s)))) a#keys;
        new variable (`Array newArr)
    | _ ->
        let `String s = to_string subject in
        new variable (`String (do_replace s))

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

let strtolower c args =
    let `String s = to_string (List.nth args 0)#get in
    new variable (`String (String.lowercase s))

let strtoupper c args =
    let `String s = to_string (List.nth args 0)#get in
    new variable (`String (String.uppercase s))

let trim context args =
    let `String s = to_string (List.nth args 0)#get in
    let `String chars = try to_string (List.nth args 0)#get with Not_found -> `String " \t\n\r\x00\x0B" in (* TODO expand .. *)
    new variable (`String (ltrim_real (rtrim_real s chars) chars))

let _ = Interpreter.Extension.register
    "core/string"
    []
    [
        ("ltrim", ltrim);
        ("rtrim", rtrim);
        ("str_replace", str_replace);
        ("strlen", strlen);
        ("strpos", strpos);
        ("strrpos", strrpos);
        ("strtolower", strtolower);
        ("strtoupper", strtoupper);
        ("trim", trim)
    ]
    []
    []

