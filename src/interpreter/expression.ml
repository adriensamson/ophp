open Language.Typing
open Language.Ast

exception MissingArrayOffset

let eval_binary opLong opDouble val1 val2 = match (to_numeric val1, to_numeric val2) with
    | (`Long a, `Long b) -> `Long (opLong a b)
    | (`Long a, `Double b) -> `Double (opDouble (float_of_int a) b)
    | (`Double a, `Long b) -> `Double (opDouble a (float_of_int b))
    | (`Double a, `Double b) -> `Double (opDouble a b)

let boolean_operator op val1 val2 =
    let `Bool b1 = to_bool val1
    and `Bool b2 = to_bool val2 in
    `Bool (op b1 b2)

let bitwise_operator op val1 val2 =
    let `Long i1 = to_long val1
    and `Long i2 = to_long val2 in
    `Long (op i1 i2)

let compare_values val1 val2 = match val1, val2 with
    | (`Array a1, `Array a2) ->
        let compare_counts = compare (a1#count ()) (a2#count ()) in
        if compare_counts = 0 then begin
            a1#rewind ();
            let result = ref (Some 0) in
            while !result = Some 0 && a1#valid () do
                let key = a1#key () in
                if a2#offsetExists key then
                    result := Some (compare (a1#current ()) (a2#offsetGet key))
                else
                    result := None;
                a1#next ();
            done;
            !result
        end else
            Some compare_counts
    | (`Array _, _) -> Some 1
    | (_, `Array _) -> Some (-1)
    | _ when is_numeric false val1 && is_numeric false val2 -> begin match to_numeric val1, to_numeric val2 with
        | (`Long i1, `Long i2) -> Some (compare i1 i2)
        | (`Double f1, `Double f2) -> Some (compare f1 f2)
        | (`Long i1, `Double f2) -> Some (compare (float_of_int i1) f2)
        | (`Double f1, `Long i2) -> Some (compare f1 (float_of_int i2))
    end
    | _ -> let `String s1 = to_string val1 and `String s2 = to_string val2 in Some (compare s1 s2)

let rec compare_all op val1 val2 = match op with
    | NotEqual -> not (compare_all Equal val1 val2)
    | NotIdentical -> not (compare_all Identical val1 val2)
    | Identical -> begin match val1, val2 with
        | (`Array a1, `Array a2) ->
            a1#rewind ();
            a2#rewind ();
            let result = ref (a1#key () = a2#key () && compare_all Identical (a1#current ()) (a2#current ())) in
            while !result do
                a1#next ();
                a2#next ();
                result := (not (a1#valid ()) && not (a2#valid ()))
                    || (a1#valid() && a2#valid()
                        && a1#key () = a2#key ()
                        && compare_all Identical (a1#current ()) (a2#current ())
                        )
            done;
            !result
        | _ -> val1 = val2
        end
    | _ -> match compare_values val1 val2 with
        | None -> false
        | Some cc -> match op with
            | Equal -> cc = 0
            | LesserEqual -> cc <= 0
            | Lesser -> cc < 0
            | GreaterEqual -> cc >= 0
            | Greater -> cc > 0
            | NotEqual | NotIdentical | Identical -> assert false

let rec eval v e = match e with
    | ConstValue f -> f
    | ConcatList l -> `String (String.concat "" (List.map (fun e -> let `String s = to_string (eval v e) in s) l))
    | Assignable a -> eval_assignable v a
    | BinaryOperation (op, f, g) -> begin match op with
        | Plus -> eval_binary (+) (+.) (eval v f) (eval v g)
        | Minus -> eval_binary (-) (-.) (eval v f) (eval v g)
        | Mult -> eval_binary ( * ) ( *. ) (eval v f) (eval v g)
        | Div -> eval_binary (/) (/.) (eval v f) (eval v g)
        | Modulo -> eval_binary (mod) (mod_float) (eval v f) (eval v g)
        | Concat -> let `String s1 = to_string (eval v f) and `String s2 = to_string (eval v g) in `String (s1 ^ s2)
        | BitwiseAnd -> bitwise_operator (land) (eval v f) (eval v g)
        | BitwiseOr -> bitwise_operator (lor) (eval v f) (eval v g)
        | BitwiseXor -> bitwise_operator (lxor) (eval v f) (eval v g)
        | ShiftLeft -> bitwise_operator (lsl) (eval v f) (eval v g)
        | ShiftRight -> bitwise_operator (lsr) (eval v f) (eval v g)
    end
    | And (f, g) -> boolean_operator (&&) (eval v f) (eval v g)
    | Or (f, g) -> boolean_operator (||) (eval v f) (eval v g)
    | Xor (f, g) -> boolean_operator (!=) (eval v f) (eval v g)
    | Not f -> let `Bool b = to_bool (eval v f) in `Bool (not b)
    | Comparison (op, f, g) -> `Bool (compare_all op (eval v f) (eval v g))
    | FunctionCall (name, argValues) -> Registry.functions#exec name (List.map (eval v) argValues)
    | ArrayConstructor l -> let phpArray = new PhpArray.phpArray in
        let addElement (e1, e2) = match eval v e1 with
            | `Null -> phpArray#offsetSet None (eval v e2)
            | o -> let `String offset = to_string o in phpArray#offsetSet (Some offset) (eval v e2)
        in
        List.iter addElement l;
        `Array phpArray
    | Assign (a, f) -> begin match a with
        | Variable s -> let g = eval v f in v#set s g; g
        | VariableVariable e -> let `String s = to_string (eval v e) in let g = eval v f in v#set s g; g
        | ArrayOffset (a, o) ->
            let arr = match eval_assignable v a with `Array arr -> arr | _ -> raise BadType in
            let value = eval v f in
            begin match o with
                | None -> arr#offsetSet None value
                | Some o -> let `String offset = to_string (eval v o) in arr#offsetSet (Some offset) value
            end; value
    | StaticProperty (className, propName) -> (Registry.classes# get className)#getStaticProperty propName
    end
    | BinaryAssign (op, a, f) -> eval v (Assign (a, BinaryOperation (op, Assignable a, f)))
    | PreInc a -> eval v (Assign (a, BinaryOperation (Plus, Assignable a, ConstValue (`Long 1))))
    | PostInc a -> let ret = eval v (Assignable a) in let _ = eval v (Assign (a, BinaryOperation (Plus, Assignable a, ConstValue (`Long 1)))) in ret
    | PreDec a -> eval v (Assign (a, BinaryOperation (Minus, Assignable a, ConstValue (`Long 1))))
    | PostDec a -> let ret = eval v (Assignable a) in let _ = eval v (Assign (a, BinaryOperation (Minus, Assignable a, ConstValue (`Long 1)))) in ret
and eval_assignable v a = match a with
    | Variable s -> v#get s
    | VariableVariable e -> let `String s = to_string (eval v e) in v#get s
    | ArrayOffset (a, o) -> begin
        match o with
            | None -> raise MissingArrayOffset
            | Some o -> match eval_assignable v a with
                | `Array a -> let `String offset = to_string (eval v o) in a#offsetGet offset
                | _ -> raise BadType
    end

