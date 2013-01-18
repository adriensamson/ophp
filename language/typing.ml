class ['a, 'b] phpArray =
    object
        val hashTable = (Hashtbl.create 10 : ('a, 'b) Hashtbl.t)
        val mutable indexList = []
        val mutable currentIndex = None
        val mutable nextNumericOffset = 0
        method offsetExists off = Hashtbl.mem hashTable off
        method offsetGet off = Hashtbl.find hashTable off
        method offsetSet off value =
            let offset = match off with
                | Some s ->
                    if Str.string_match (Str.regexp "[0-9]+") s 0 then
                        let i = int_of_string (Str.matched_string s) in
                        nextNumericOffset <- max nextNumericOffset (i+1)
                    else ();
                    s
                | None ->
                    let s = string_of_int nextNumericOffset in
                    nextNumericOffset <- nextNumericOffset + 1;
                    s
            in
            if not (List.mem offset indexList) then
                indexList <- indexList @ [offset]
            else ();
            Hashtbl.replace hashTable offset value
        method offsetUnset off =
            let rec list_remove l e = match l with
                | [] -> []
                | a::t when a = e -> t
                | a::t -> a::(list_remove t e)
            in
            indexList <- list_remove indexList off;
            Hashtbl.remove hashTable off
        
        method current () = match currentIndex with
            | None -> raise Not_found
            | Some k -> Hashtbl.find hashTable k
        method key () = match currentIndex with
            | None -> raise Not_found
            | Some k -> k
        method next () = match currentIndex with
            | None -> ()
            | Some a -> let rec do_next l = match l with
                | [] -> assert false
                | [b] when b = a -> currentIndex <- None
                | [_] -> assert false
                | b::c::_ when b = a -> currentIndex <- Some c
                | b::t -> do_next t
            in do_next indexList
        method rewind () = match indexList with
            | [] -> currentIndex <- None
            | a::_ -> currentIndex <- Some a
        method valid () = match currentIndex with 
            | None -> false
            | Some _ -> true
        
        method count () = List.length indexList
    end

exception BadType

type value = [
    | `Null
    | `Bool of bool
    | `Double of float
    | `Long of int
    | `String of string
    | `Array of (string, value) phpArray
]

let is_numeric strict (v:value) = match v with
    | `Null | `Bool _ -> not strict
    | `Double _ | `Long _ -> true
    | `Array _ (*| `Object _*) -> false
    | `String s -> let r = Str.regexp "^[-+]?\\([0-9]+|[0-9]+\\.[0-9]*|\\.[0-9]+\\)$" in Str.string_match r s 0

let to_numeric (v:value) = match v with
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
    | `Array _ -> raise BadType

let to_string (v:value) = match v with
    | `Null -> `String ""
    | `Bool false -> `String ""
    | `Bool true -> `String "1"
    | `Double d -> `String (string_of_float d)
    | `Long l -> `String (string_of_int l)
    | `String s -> `String s
    | `Array _ -> raise BadType

let to_bool (v:value) = match v with
    | `Null -> `Bool false
    | `Bool b -> `Bool b
    | `Double d -> `Bool (d <> 0.)
    | `Long l -> `Bool (l <> 0)
    | `String s -> `Bool (not (s = "" || s = "0"))
    | `Array _ -> raise BadType

let to_long (v:value) = match v with
    | `Null -> `Long 0
    | `Bool true -> `Long 1
    | `Bool false -> `Long 0
    | `Double d -> `Long (int_of_float d)
    | `Long l -> `Long l
    | `String s ->
        let r = Str.regexp "[0-9]+" in
        let matches = Str.string_match r s 0 in
        if matches then `Long (int_of_string (Str.matched_string s)) else `Long 0
    | `Array _ -> raise BadType

