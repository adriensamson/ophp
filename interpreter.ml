open Language.Typing
open Language.Ast

let functions = Hashtbl.create 10

let eval_binary opLong opDouble val1 val2 = match (val1, val2) with
    | (Long a, Long b) -> Long (opLong a b)
    | (Long a, Double b) -> Double (opDouble (float_of_int a) b)
    | (Double a, Long b) -> Double (opDouble a (float_of_int b))
    | (Double a, Double b) -> Double (opDouble a b)

let rec eval v e = match e with
    | ConstValue f -> f
    | Plus (f, g) -> eval_binary (+) (+.) (eval v f) (eval v g)
    | Minus (f, g) -> eval_binary (-) (-.) (eval v f) (eval v g)
    | Mult (f, g) -> eval_binary ( * ) ( *. ) (eval v f) (eval v g)
    | Div (f, g) -> eval_binary (/) (/.) (eval v f) (eval v g)
    | Variable s -> Hashtbl.find v s
    | FunctionCall (name, argValues) -> let (argNames, code) = Hashtbl.find functions name in
        let local_vars = Hashtbl.create 10 in
        List.iter2 (fun name value -> Hashtbl.add local_vars name (eval v value)) argNames argValues;
        List.iter (exec local_vars) code; Long 0

and exec v s = match s with
    | IgnoreResult e -> let _ = eval v e in ()
    | FunctionDef (name, argList, code) -> Hashtbl.add functions name (argList, code)
    | Echo e -> begin match (eval v e) with
            | Double f -> print_float f
            | Long i -> print_int i
        end;
        print_newline ()

