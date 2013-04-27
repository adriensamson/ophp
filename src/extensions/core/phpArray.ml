open Language.Typing

let array_push c l =
    let a = match (List.nth l 0)#get with `Array a -> a | _ -> failwith "Not an array" in
    let v = (List.nth l 1)#get in
    a#offsetVarSet a#nextOffset (new variable v);
    new variable (`Long (a#count ()))

let implode c l =
    let `String sep = to_string (List.nth l 0)#get in
    let a = match (List.nth l 1)#get with `Array a -> a | _ -> failwith "Not an array" in
    let s = String.concat sep (List.map (fun k -> let `String s = to_string (a#offsetVar k)#get in s) a#keys) in
    new variable (`String s)

let _ = Interpreter.Extension.register
    "core/array"
    []
    [
        ("array_push", array_push);
        ("implode", implode);("join", implode)
    ]
    []
    []

