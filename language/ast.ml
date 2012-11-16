type expr =
    | ConstValue of Typing.value
    | Plus of expr * expr
    | Minus of expr * expr
    | Mult of expr * expr
    | Div of expr * expr
    | FunctionCall of string * expr list
    | Variable of string

type stmt =
    | Echo of expr
    | Return of expr
    | IgnoreResult of expr
    | FunctionDef of string * string list * stmt list
