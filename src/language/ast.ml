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

type name =
    | FullyQualifiedName of string list * string
    | RelativeName of string list * string

type classReference =
    | ClassName of name
    | Parent
    | Self
    | Static

type constValues = [
    | `Null
    | `Bool of bool
    | `Double of float
    | `Long of int
    | `String of string
]

type expr =
    | ConstValue of constValues
    | Constant of string
    | ConcatList of  expr list
    | Assignable of  assignable
    | Closure of bool * (string * bool) list * (string * bool) list * stmt list
    | This
    | FunctionCall of name *  expr list
    | Invoke of expr * expr list
    | MethodCall of  expr * string *  expr list
    | StaticMethodCall of classReference * string *  expr list
    | ClassConstant of classReference * string
    | BinaryOperation of binaryOperator *  expr *  expr
    | UnaryMinus of expr
    | Comparison of comparisonOperator *  expr *  expr
    | And of  expr *  expr
    | Or of  expr *  expr
    | Xor of  expr *  expr
    | Not of  expr
    | Assign of  assignable *  expr
    | AssignByRef of assignable * expr
    | BinaryAssign of binaryOperator *  assignable *  expr
    | PreInc of  assignable
    | PostInc of  assignable
    | PreDec of  assignable
    | PostDec of  assignable
    | ArrayConstructor of (expr option *  expr) list
    | NewObject of name *  expr list
    | Include of  expr * bool * bool (* filename, required, once *)
and  assignable =
    | Variable of string
    | VariableVariable of  expr
    | ArrayOffset of  expr *  expr option
    | Property of  expr * string
    | StaticProperty of classReference * string
and  stmt =
    | Global of string
    | Echo of  expr
    | Return of  expr
    | IgnoreResult of  expr
    | FunctionDef of string * bool * (string * bool) list *  stmt list
    | ClassDef of string * bool * bool * bool * bool * string option * string list *  classDefElement list (* name, isStatic, isAbstract, isFinal, isInterface, parent, implements, content *)
    | If of  expr *  stmt list
    | IfElse of  expr *  stmt list *  stmt list
    | While of  expr *  stmt list
    | For of  expr list *  expr list *  expr list *  stmt list
    | Foreach of  expr * string option * string *  stmt list
    | Break of int
    | Continue of int
and  classDefElement =
    | ConstantDef of string *  expr
    | PropertyDef of string * bool * Typing.visibility *  expr option
    | MethodDef of string * bool * bool * Typing.visibility * (string * bool) list *  stmt list
    | AbstractMethodDef of string * bool * Typing.visibility * string list

type namespaceStmt =
    | NamespaceBlock of string list * (string list * string option) list * stmt list

