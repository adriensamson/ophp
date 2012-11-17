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

let to_bool (v:value) = match v with
    | `Null -> `Bool false
    | `Bool b -> `Bool b
    | `Double d -> `Bool (d <> 0.)
    | `Long l -> `Bool (l <> 0)

let to_long (v:value) = match v with
    | `Null -> `Long 0
    | `Bool true -> `Long 1
    | `Bool false -> `Long 0
    | `Double d -> `Long (int_of_float d)
    | `Long l -> `Long l

