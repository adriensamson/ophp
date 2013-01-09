type binaryOperator =
    | Plus
    | Minus
    | Mult
    | Div
    | Modulo
    | Concat
    | BitwiseAnd
    | BitwiseOr
    | BitwiseXor
    | ShiftLeft
    | ShiftRight

type comparisonOperator =
    | GreaterEqual
    | Greater
    | Lesser
    | LesserEqual
    | Equal
    | Identical
    | NotEqual
    | NotIdentical

type expr =
    | ConstValue of Typing.value
    | Assignable of assignable
    | FunctionCall of string * expr list
    | BinaryOperation of binaryOperator * expr * expr
    | Comparison of comparisonOperator * expr * expr
    | And of expr * expr
    | Or of expr * expr
    | Xor of expr * expr
    | Not of expr
    | Assign of assignable * expr
    | BinaryAssign of binaryOperator * assignable * expr
    | PreInc of assignable
    | PostInc of assignable
    | PreDec of assignable
    | PostDec of assignable
    | ArrayConstructor of (expr * expr) list
and assignable =
    | Variable of string
    | ArrayOffset of assignable * expr option

type stmt =
    | Echo of expr
    | Return of expr
    | IgnoreResult of expr
    | FunctionDef of string * string list * stmt list
