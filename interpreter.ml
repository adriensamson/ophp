open Language.Typing
open Language.Ast

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
    | Plus (f, g) -> eval_binary (+) (+.) (eval v f) (eval v g)
    | Minus (f, g) -> eval_binary (-) (-.) (eval v f) (eval v g)
    | Mult (f, g) -> eval_binary ( * ) ( *. ) (eval v f) (eval v g)
    | Div (f, g) -> eval_binary (/) (/.) (eval v f) (eval v g)
    | Mod (f, g) -> eval_binary (mod) (mod_float) (eval v f) (eval v g)
    | And (f, g) -> boolean_operator (&&) (eval v f) (eval v g)
    | Or (f, g) -> boolean_operator (||) (eval v f) (eval v g)
    | Xor (f, g) -> boolean_operator (!=) (eval v f) (eval v g)
    | Not f -> let `Bool b = to_bool (eval v f) in `Bool (not b)
    | BitwiseAnd (f, g) -> bitwise_operator (land) (eval v f) (eval v g)
    | BitwiseOr (f, g) -> bitwise_operator (lor) (eval v f) (eval v g)
    | BitwiseXor (f, g) -> bitwise_operator (lxor) (eval v f) (eval v g)
    | Assign (s, f) -> let g = eval v f in Hashtbl.replace v s g; g
    | Variable s -> Hashtbl.find v s
    | FunctionCall (name, argValues) -> let (argNames, code) = Hashtbl.find functions name in
        let local_vars = Hashtbl.create 10 in
        List.iter2 (fun name value -> Hashtbl.add local_vars name (eval v value)) argNames argValues;
        match exec_list local_vars code with
            | NoOp -> `Null
            | Return v -> v

and exec v s = match s with
    | IgnoreResult e -> let _ = eval v e in NoOp
    | Language.Ast.Return e -> Return (eval v e)
    | FunctionDef (name, argList, code) -> Hashtbl.add functions name (argList, code); NoOp
    | Echo e -> begin match (eval v e) with
            | `Null -> ()
            | `Bool b -> print_string (string_of_bool b)
            | `Double f -> print_float f
            | `Long i -> print_int i
        end;
        print_newline ();
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
