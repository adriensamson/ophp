open Language.Typing

let array_keys c l =
    let a = match (List.nth l 0)#get with `Array a -> a | _ -> failwith "Not an array" in
    let akeys = new Interpreter.PhpArray.phpArray in
    List.iter 
        (fun k -> akeys#set akeys#nextOffset (new variable (`String k)))
        a#keys;
    new variable (`Array akeys)

let array_map c l = (* TODO mulitple arrays *)
    let a = match (List.nth l 1)#get with `Array a -> a | _ -> failwith "Not an array" in
    let callable = (List.nth l 0)#get in
    let newArr = new Interpreter.PhpArray.phpArray in
    List.iter
        (fun k -> newArr#set k (Interpreter.Compiler.invoke_callable callable c [a#get k]))
        a#keys;
    new variable (`Array newArr)

let array_merge c l =
    let a = match (List.nth l 0)#get with `Array a -> a | _ -> failwith "Not an array" in
    let newArr = a#copy () in
    List.iter
        (fun v ->
            let a = match v#get with `Array a -> a | _ -> failwith "Not an array" in
            List.iter
                (fun k -> if is_numeric false (`String k) then newArr#set newArr#nextOffset (a#get k) else newArr#set k (a#get k))
                a#keys
        )
        (List.tl l)
        ;
    new variable (`Array newArr)

let array_push c l =
    let a = match (List.nth l 0)#get with `Array a -> a | _ -> failwith "Not an array" in
    let v = (List.nth l 1)#get in
    a#set a#nextOffset (new variable v);
    new variable (`Long (a#count ()))

let array_shift c l =
    let a = match (List.nth l 0)#get with `Array a -> a | _ -> failwith "Not an array" in
    match a#keys with
    | [] -> new variable `Null
    | h::t ->
        let r = a#get h in
        let newArr = new Interpreter.PhpArray.phpArray in
        List.iter
            (fun k -> if is_numeric false (`String k) then newArr#set newArr#nextOffset (a#get k) else newArr#set k (a#get k))
            t;
        newArr#rewind ();
        (List.nth l 0)#set (`Array newArr);
        r

let array_slice c l =
    let a = match (List.nth l 0)#get with `Array a -> a | _ -> failwith "Not an array" in
    let `Long offset = to_long (List.nth l 1)#get in
    let length = try (List.nth l 2)#get with Failure _ -> `Null in
    let `Bool preserveKeys = try to_bool (List.nth l 3)#get with Failure _ -> `Bool false in
    
    let arrayLength = a#count () in
    let realOffset = if offset >= 0 then offset else
        max (arrayLength + offset) 0
    in
    let calcLength = match length with 
        | `Null -> arrayLength - realOffset
        | v -> let `Long l = to_long v in if l >= 0 then l else arrayLength + l - realOffset
    in
    let realLength = max 0 calcLength in
    let rec slice l offset length = match l, offset, length with
        | h::t, i, _ when i > 0 -> slice t (i - 1) length
        | h::t, _, i when i > 0 -> h::(slice t 0 (i - 1))
        | _, _, _ -> []
    in
    let newArr = new Interpreter.PhpArray.phpArray in
    List.iter
        (fun k -> if is_numeric false (`String k) && not preserveKeys then newArr#set newArr#nextOffset (a#get k) else newArr#set k (a#get k))
        (slice a#keys realOffset realLength)
    ;
    new variable (`Array newArr)

let each c l =
    let a = match (List.nth l 0)#get with `Array a -> a | _ -> failwith "Not an array" in
    if a#valid () then
        let newArr = new Interpreter.PhpArray.phpArray in
        newArr#set "0" (new variable (`String (a#key ())));
        newArr#set "1" (new variable (a#current ())#get);
        newArr#set "key" (new variable (`String (a#key ())));
        newArr#set "value" (new variable (a#current ())#get);
        a#next ();
        new variable (`Array newArr)
    else
        new variable (`Bool false)

let implode c l =
    let `String sep = to_string (List.nth l 0)#get in
    let a = match (List.nth l 1)#get with `Array a -> a | _ -> failwith "Not an array" in
    let s = String.concat sep (List.map (fun k -> let `String s = to_string (a#get k)#get in s) a#keys) in
    new variable (`String s)

let reset c l =
    let a = match (List.nth l 0)#get with `Array a -> a | _ -> failwith "Not an array" in
    a#rewind ();
    a#current ()

let sort c l =
    let a = match (List.nth l 0)#get with `Array a -> a | _ -> failwith "Not an array" in
    a#sort (Interpreter.PhpArray.sortCompare a);
    List.nth l 0

let _ = Interpreter.Extension.register
    "core/array"
    []
    [
        ("array_keys", array_keys);
        ("array_map", array_map);
        ("array_merge", array_merge);
        ("array_push", array_push);
        ("array_shift", array_shift);
        ("array_slice", array_slice);
        ("each", each);
        ("implode", implode);
        ("join", implode);
        ("reset", reset);
        ("sort", sort)
    ]
    []
    []

