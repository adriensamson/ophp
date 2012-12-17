type expr =
    | ConstValue of Typing.value
    | Variable of string
    | FunctionCall of string * expr list
    | Plus of expr * expr
    | Minus of expr * expr
    | Mult of expr * expr
    | Div of expr * expr
    | Mod of expr * expr
    | Concat of expr * expr
    | And of expr * expr
    | Or of expr * expr
    | Xor of expr * expr
    | BitwiseAnd of expr * expr
    | BitwiseOr of expr * expr
    | BitwiseXor of expr * expr
    | Not of expr
    | Assign of string * expr
    | ArrayConstructor of (expr * expr) list
    | ArrayOffsetGet of expr * expr
    | ArrayOffsetSet of string * expr list * expr * expr

type stmt =
    | Echo of expr
    | Return of expr
    | IgnoreResult of expr
    | FunctionDef of string * string list * stmt list
