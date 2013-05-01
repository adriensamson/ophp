open Language.Typing

let array_keys c l =
    let a = match (List.nth l 0)#get with `Array a -> a | _ -> failwith "Not an array" in
    let akeys = new Interpreter.PhpArray.phpArray in
    List.iter 
        (fun k -> akeys#offsetVarSet akeys#nextOffset (new variable (`String k)))
        a#keys;
    new variable (`Array akeys)

let array_map c l = (* TODO mulitple arrays *)
    let a = match (List.nth l 1)#get with `Array a -> a | _ -> failwith "Not an array" in
    let callable = (List.nth l 0)#get in
    let newArr = new Interpreter.PhpArray.phpArray in
    List.iter
        (fun k -> newArr#offsetVarSet k (Interpreter.Compiler.invoke_callable callable c [a#offsetVar k]))
        a#keys;
    new variable (`Array newArr)

let array_push c l =
    let a = match (List.nth l 0)#get with `Array a -> a | _ -> failwith "Not an array" in
    let v = (List.nth l 1)#get in
    a#offsetVarSet a#nextOffset (new variable v);
    new variable (`Long (a#count ()))

let array_shift c l =
    let a = match (List.nth l 0)#get with `Array a -> a | _ -> failwith "Not an array" in
    match a#keys with
    | [] -> new variable `Null
    | h::t ->
        let r = a#offsetVar h in
        let newArr = new Interpreter.PhpArray.phpArray in
        List.iter
            (fun k -> if is_numeric false (`String k) then newArr#offsetVarSet newArr#nextOffset (a#offsetVar k) else newArr#offsetVarSet k (a#offsetVar k))
            t;
        newArr#rewind ();
        (List.nth l 0)#set (`Array newArr);
        r

let each c l =
    let a = match (List.nth l 0)#get with `Array a -> a | _ -> failwith "Not an array" in
    if a#valid () then
        let newArr = new Interpreter.PhpArray.phpArray in
        newArr#offsetVarSet "0" (new variable (`String (a#key ())));
        newArr#offsetVarSet "1" (new variable (a#current ())#get);
        newArr#offsetVarSet "key" (new variable (`String (a#key ())));
        newArr#offsetVarSet "value" (new variable (a#current ())#get);
        a#next ();
        new variable (`Array newArr)
    else
        new variable (`Bool false)

let implode c l =
    let `String sep = to_string (List.nth l 0)#get in
    let a = match (List.nth l 1)#get with `Array a -> a | _ -> failwith "Not an array" in
    let s = String.concat sep (List.map (fun k -> let `String s = to_string (a#offsetVar k)#get in s) a#keys) in
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
        ("array_push", array_push);
        ("array_shift", array_shift);
        ("each", each);
        ("implode", implode);
        ("join", implode);
        ("reset", reset);
        ("sort", sort)
    ]
    []
    []

