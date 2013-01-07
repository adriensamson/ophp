open Language.Typing
open Language.Ast

exception MissingArrayOffset

type exec_return =
    | NoOp
    | Return of value

let functions = Hashtbl.create 10

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

let rec eval v e = match e with
    | ConstValue f -> f
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
    | FunctionCall (name, argValues) -> begin
        let (argNames, code) = Hashtbl.find functions name in
            let local_vars = Hashtbl.create 10 in
            List.iter2 (fun name value -> Hashtbl.add local_vars name (eval v value)) argNames argValues;
            match exec_list local_vars code with
                | NoOp -> `Null
                | Return v -> v
    end
    | ArrayConstructor l -> let phpArray = new phpArray in
        let addElement (e1, e2) = match eval v e1 with
            | `Null -> phpArray#offsetSet None (eval v e2)
            | o -> let `String offset = to_string o in phpArray#offsetSet (Some offset) (eval v e2)
        in
        List.iter addElement l;
        `Array phpArray
    | Assign (a, f) -> begin match a with
        | Variable s -> let g = eval v f in Hashtbl.replace v s g; g
        | ArrayOffset (a, o) ->
            let arr = match eval_assignable v a with `Array arr -> arr | _ -> raise BadType in
            let value = eval v f in
            begin match o with
                | None -> arr#offsetSet None value
                | Some o -> let `String offset = to_string (eval v o) in arr#offsetSet (Some offset) value
            end; value
    end
    | BinaryAssign (op, a, f) -> eval v (Assign (a, BinaryOperation (op, Assignable a, f)))
and eval_assignable v a = match a with
    | Variable s -> Hashtbl.find v s
    | ArrayOffset (a, o) -> begin
        match o with
            | None -> raise MissingArrayOffset
            | Some o -> match eval_assignable v a with
                | `Array a -> let `String offset = to_string (eval v o) in a#offsetGet offset
                | _ -> raise BadType
    end
        
and exec v s = match s with
    | IgnoreResult e -> let _ = eval v e in NoOp
    | Language.Ast.Return e -> Return (eval v e)
    | FunctionDef (name, argList, code) -> Hashtbl.add functions name (argList, code); NoOp
    | Echo e -> begin match (eval v e) with
            | `Null -> ()
            | `String s -> print_string s
            | `Bool b -> print_string (string_of_bool b)
            | `Double f -> print_float f
            | `Long i -> print_int i
            | `Array _ -> print_string "Array(...)"
        end;
        NoOp

and exec_list v sl = match sl with
    | [] -> NoOp
    | a::t -> match exec v a with
        | NoOp -> exec_list v t
        | r -> r

let run stmt_list = 
    let variables = Hashtbl.create 10 in
    let _ = exec_list variables stmt_list in
    ()
