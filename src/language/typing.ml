type visibility =
    | Public
    | Protected
    | Private

exception BadType
exception BadVisibility

type ('a, 'o) value = [
    | `Null
    | `Bool of bool
    | `Double of float
    | `Long of int
    | `String of string
    | `Array of 'a
    | `Object of 'o
]

let is_numeric strict (v : (_, _) value) = match v with
    | `Null | `Bool _ -> not strict
    | `Double _ | `Long _ -> true
    | `Array _ | `Object _ -> false
    | `String s -> let r = Str.regexp "^[-+]?\\([0-9]+|[0-9]+\\.[0-9]*|\\.[0-9]+\\)$" in Str.string_match r s 0

let to_numeric (v : (_, _) value) = match v with
    | `Null -> `Long 0
    | `Bool true -> `Long 1
    | `Bool false -> `Long 0
    | `Double d -> `Double d
    | `Long l -> `Long l
    | `String s ->
        let rInt = Str.regexp "[-+]?[0-9]+"
        and rFloat = Str.regexp "[-+]?[0-9]+\\.[0-9]*|[-+]?\\.[0-9]+" in
        if Str.string_match rFloat s 0 then
            `Double (float_of_string (Str.matched_string s))
        else if Str.string_match rInt s 0 then
            `Long (int_of_string (Str.matched_string s))
        else
            `Long 0
    | `Array _ | `Object _ -> raise BadType

let to_string (v : (_, _) value) = match v with
    | `Null -> `String ""
    | `Bool false -> `String ""
    | `Bool true -> `String "1"
    | `Double d ->
        let i = int_of_float d in
        if float_of_int i = d then
            `String (string_of_int i)
        else
            `String (Printf.sprintf "%.14g" d)
    | `Long l -> `String (string_of_int l)
    | `String s -> `String s
    | `Array _ | `Object _ -> raise BadType

let to_bool (v : (_, _) value) = match v with
    | `Null -> `Bool false
    | `Bool b -> `Bool b
    | `Double d -> `Bool (d <> 0.)
    | `Long l -> `Bool (l <> 0)
    | `String s -> `Bool (not (s = "" || s = "0"))
    | `Array _ | `Object _ -> raise BadType

let to_long (v : (_, _) value) = match v with
    | `Null -> `Long 0
    | `Bool true -> `Long 1
    | `Bool false -> `Long 0
    | `Double d -> `Long (int_of_float d)
    | `Long l -> `Long l
    | `String s ->
        let r = Str.regexp "[0-9]+" in
        let matches = Str.string_match r s 0 in
        if matches then `Long (int_of_string (Str.matched_string s)) else `Long 0
    | `Array _ | `Object _ -> raise BadType

