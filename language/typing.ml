type value = [
    | `Null
    | `Bool of bool
    | `Double of float
    | `Long of int
]

let to_numeric (v:value) = match v with
    | `Null -> `Long 0
    | `Bool true -> `Long 1
    | `Bool false -> `Long 0
    | `Double d -> `Double d
    | `Long l -> `Long l

