type expr =
    | Num of float
    | Plus of expr * expr
    | Minus of expr * expr
    | Mult of expr * expr
    | Div of expr * expr
    | Umin of expr

type stmt =
    | Nothing
    | Echo of expr

let rec eval e = match e with
    | Num f -> f
    | Plus (f, g) -> eval f +. eval g
    | Minus (f, g) -> eval f -. eval g
    | Mult (f, g) -> eval f *. eval g
    | Div (f, g) -> eval f /. eval g
    | Umin f -> -. eval f

let exec s = match s with
    | Nothing -> ()
    | Echo e -> print_float (eval e); print_newline ()

;;
