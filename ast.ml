type value =
    | Double of float
    | Long of int

type expr =
    | ConstValue of value
    | Plus of expr * expr
    | Minus of expr * expr
    | Mult of expr * expr
    | Div of expr * expr
    | FunctionCall of string * expr list

type stmt =
    | Echo of expr
    | IgnoreResult of expr
    | FunctionDef of string * string list * stmt list

let functions = Hashtbl.create 10

let eval_binary opLong opDouble val1 val2 = match (val1, val2) with
    | (Long a, Long b) -> Long (opLong a b)
    | (Long a, Double b) -> Double (opDouble (float_of_int a) b)
    | (Double a, Long b) -> Double (opDouble a (float_of_int b))
    | (Double a, Double b) -> Double (opDouble a b)

let rec eval e = match e with
    | ConstValue f -> f
    | Plus (f, g) -> eval_binary (+) (+.) (eval f) (eval g)
    | Minus (f, g) -> eval_binary (-) (-.) (eval f) (eval g)
    | Mult (f, g) -> eval_binary ( * ) ( *. ) (eval f) (eval g)
    | Div (f, g) -> eval_binary (/) (/.) (eval f) (eval g)
    | FunctionCall (name, argList) -> let (_, code) = Hashtbl.find functions name in List.iter exec code; Long 0

and exec s = match s with
    | IgnoreResult e -> let _ = eval e in ()
    | FunctionDef (name, argList, code) -> Hashtbl.add functions name (argList, code)
    | Echo e -> begin match (eval e) with
            | Double f -> print_float f
            | Long i -> print_int i
        end;
        print_newline ()

;;
