open Language.Typing
open Language.Ast

exception MissingArrayOffset

let eval_binary opLong opDouble val1 val2 = match (to_numeric val1, to_numeric val2) with
    | (`Long a, `Long b) -> `Long (opLong a b)
    | (`Long a, `Double b) -> `Double (opDouble (float_of_int a) b)
    | (`Double a, `Long b) -> `Double (opDouble a (float_of_int b))
    | (`Double a, `Double b) -> `Double (opDouble a b)

let eval_div val1 val2 = let f = match (to_numeric val1, to_numeric val2) with
    | (`Long a, `Long b) -> float_of_int a /. float_of_int b
    | (`Long a, `Double b) -> float_of_int a /. b
    | (`Double a, `Long b) -> a /. float_of_int b
    | (`Double a, `Double b) -> a /. b
    in
    if float_of_int (int_of_float f) = f then
        `Long (int_of_float f)
    else
        `Double f

let boolean_operator op val1 val2 =
    let `Bool b1 = to_bool val1
    and `Bool b2 = to_bool val2 in
    `Bool (op b1 b2)

let bitwise_operator op val1 val2 =
    let `Long i1 = to_long val1
    and `Long i2 = to_long val2 in
    `Long (op i1 i2)

let compare_values val1 val2 = match val1, val2 with
    | (`Array a1, `Array a2) ->
        let compare_counts = compare (a1#count ()) (a2#count ()) in
        if compare_counts = 0 then begin
            a1#rewind ();
            let result = ref (Some 0) in
            while !result = Some 0 && a1#valid () do
                let key = a1#key () in
                if a2#offsetExists key then
                    result := Some (compare (a1#current ()) (a2#offsetGet key))
                else
                    result := None;
                a1#next ();
            done;
            !result
        end else
            Some compare_counts
    | (`Array _, _) -> Some 1
    | (_, `Array _) -> Some (-1)
    | _ when is_numeric false val1 && is_numeric false val2 -> begin match to_numeric val1, to_numeric val2 with
        | (`Long i1, `Long i2) -> Some (compare i1 i2)
        | (`Double f1, `Double f2) -> Some (compare f1 f2)
        | (`Long i1, `Double f2) -> Some (compare (float_of_int i1) f2)
        | (`Double f1, `Long i2) -> Some (compare f1 (float_of_int i2))
    end
    | _ -> let `String s1 = to_string val1 and `String s2 = to_string val2 in Some (compare s1 s2)

let rec compare_all op val1 val2 = match op with
    | NotEqual -> not (compare_all Equal val1 val2)
    | NotIdentical -> not (compare_all Identical val1 val2)
    | Identical -> begin match val1, val2 with
        | (`Array a1, `Array a2) ->
            a1#rewind ();
            a2#rewind ();
            let result = ref (a1#key () = a2#key () && compare_all Identical (a1#current ()) (a2#current ())) in
            while !result do
                a1#next ();
                a2#next ();
                result := (not (a1#valid ()) && not (a2#valid ()))
                    || (a1#valid() && a2#valid()
                        && a1#key () = a2#key ()
                        && compare_all Identical (a1#current ()) (a2#current ())
                        )
            done;
            !result
        | _ -> val1 = val2
        end
    | _ -> match compare_values val1 val2 with
        | None -> false
        | Some cc -> match op with
            | Equal -> cc = 0
            | LesserEqual -> cc <= 0
            | Lesser -> cc < 0
            | GreaterEqual -> cc >= 0
            | Greater -> cc > 0
            | NotEqual | NotIdentical | Identical -> assert false

class type ['v] variableRegistry = object
    method replace : string -> <get : 'v; set : 'v -> unit > -> unit
    method find : string -> <get : 'v; set : 'v -> unit >
    method newScope : unit -> 'v variableRegistry
    method addFromParent : string -> unit
    method addFromGlobal : string -> unit
    end

class ['a, 'o, 'c] evalContext
    (vars : ('a, 'o) value variableRegistry)
    (functions : ('a, 'o) value Registry.functionRegistry)
    (classes : 'c Registry.classRegistry)
    (files : ('a, 'o) value Registry.fileRegistry)
    (obj: 'o option)
    (callingClass: 'c option)
    (staticClass: 'c option)
    (namespace: string list)
    (namespaceUses: (string list * string option) list)
    = object (self)
    val vars = vars
    val obj = obj
    val callingClass = callingClass
    val staticClass = staticClass
    val namespace = namespace
    val namespaceUses = namespaceUses
    method vars = vars
    method functions = functions
    method classes = classes
    method files = files
    method obj = obj
    method callingClass = callingClass
    method staticClass = staticClass
    method namespace = namespace
    method resolveNamespace ?fallbackTest n =
        let fallbackName = ref None in
        let fullNameArray = match n with
        | FullyQualifiedName (parts, name) -> parts @ [name]
        | RelativeName (parts, name) -> match parts with
            | [] -> fallbackName := Some name; self#resolveNamespaceAlias name
            | alias::t -> self#resolveNamespaceAlias alias @ t @ [name]
        in let fullName = String.concat "\\" fullNameArray in
        match fallbackTest, !fallbackName with
        | Some f, Some name ->
            if f fullName then
                fullName
            else
                name
        | _ -> fullName
    method private resolveNamespaceAlias alias =
        let rec f l = match l with
        | [] -> self#namespace @ [alias]
        | (parts, Some a)::t when a = alias -> parts @ [alias]
        | (_, Some _)::t -> f t
        | (parts, None)::t -> match List.rev parts with
            | [] -> assert false
            | a::p when a = alias -> parts
            | _ -> f t
        in f namespaceUses
    method functionScope ?(callingClass: 'c option) ?(obj: 'o option) ?(staticClass: 'c option) () =
        {<
            vars = vars#newScope ();
            callingClass = callingClass;
            obj = obj;
            staticClass = staticClass
        >}
    method namespaceScope ~(namespace: string list) ~(namespaceUses: (string list * string option) list) () =
        {<
            namespace = namespace;
            namespaceUses = namespaceUses
        >}
end

let makeContext ?obj ?callingClass ?staticClass ?(namespace=[]) ?(namespaceUses=[]) vars functions classes files =
    new evalContext vars functions classes files obj callingClass staticClass namespace namespaceUses

let getSome o = match o with
    | None -> assert false
    | Some a -> a

let convertConst c = match c with
    | `Null -> `Null
    | `Bool b -> `Bool b
    | `Double f -> `Double f
    | `Long i -> `Long i
    | `String s -> `String s

