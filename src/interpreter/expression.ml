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
    (obj: 'o option)
    (callingClass: 'c option)
    (staticClass: 'c option)
    (namespace: string list)
    (namespaceUses: (string list * string option) list)
    = object (self)
    method vars = vars
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
    end

let makeContext ?obj ?callingClass ?staticClass ?(namespace=[]) ?(namespaceUses=[]) vars =
    new evalContext vars obj callingClass staticClass namespace namespaceUses

let getSome o = match o with
    | None -> assert false
    | Some a -> a

let convertConst c = match c with
    | `Null -> `Null
    | `Bool b -> `Bool b
    | `Double f -> `Double f
    | `Long i -> `Long i
    | `String s -> `String s

class evaluator
    functionRegistry
    classRegistry
    fileRegistry
    execFile
    =
    object (self)
    method eval (v : (_, _, _) evalContext) e =
        match e with
        | ConstValue f -> convertConst f
        | ConcatList l -> `String (String.concat "" (List.map (fun e -> let `String s = to_string (self#eval v e) in s) l))
        | Assignable a -> self#eval_assignable v a
        | This -> `Object (getSome v#obj)
        | BinaryOperation (op, f, g) -> begin match op with
            | Plus -> eval_binary (+) (+.) (self#eval v f) (self#eval v g)
            | Minus -> eval_binary (-) (-.) (self#eval v f) (self#eval v g)
            | Mult -> eval_binary ( * ) ( *. ) (self#eval v f) (self#eval v g)
            | Div -> eval_div (self#eval v f) (self#eval v g)
            | Modulo -> eval_binary (mod) (mod_float) (self#eval v f) (self#eval v g)
            | Concat -> let `String s1 = to_string (self#eval v f) and `String s2 = to_string (self#eval v g) in `String (s1 ^ s2)
            | BitwiseAnd -> bitwise_operator (land) (self#eval v f) (self#eval v g)
            | BitwiseOr -> bitwise_operator (lor) (self#eval v f) (self#eval v g)
            | BitwiseXor -> bitwise_operator (lxor) (self#eval v f) (self#eval v g)
            | ShiftLeft -> bitwise_operator (lsl) (self#eval v f) (self#eval v g)
            | ShiftRight -> bitwise_operator (lsr) (self#eval v f) (self#eval v g)
        end
        | UnaryMinus f -> eval_binary (-) (-.) (`Long 0) (self#eval v f)
        | And (f, g) -> boolean_operator (&&) (self#eval v f) (self#eval v g)
        | Or (f, g) -> boolean_operator (||) (self#eval v f) (self#eval v g)
        | Xor (f, g) -> boolean_operator (!=) (self#eval v f) (self#eval v g)
        | Not f -> let `Bool b = to_bool (self#eval v f) in `Bool (not b)
        | Comparison (op, f, g) -> `Bool (compare_all op (self#eval v f) (self#eval v g))
        | FunctionCall (name, argValues) -> functionRegistry#exec (v#resolveNamespace ~fallbackTest:(functionRegistry#has) name) (List.map (fun f -> match self#eval v f with `Array a -> `Array (a#copy ()) | a -> a) argValues)
        | ClassConstant (classRef, constantName) ->
            let phpClass = match classRef with
                | ClassName className -> classRegistry#get (v#resolveNamespace className)
                | Self -> getSome v#callingClass
                | Parent -> getSome (getSome v#callingClass)#parent
                | Static -> getSome v#staticClass
            in Object.getClassConstant phpClass constantName
        | MethodCall (obj, methodName, argValues) -> begin match self#eval v obj with
            | `Object o -> Object.getObjectMethod o v#callingClass methodName (List.map (fun f -> match self#eval v f with `Array a -> `Array (a#copy ()) | a -> a) argValues)
            | _ -> raise BadType
        end
        | StaticMethodCall (classRef, methodName, argValues) -> begin
            let (phpClass, staticClass) = match classRef with
                | ClassName className -> let c = classRegistry#get (v#resolveNamespace className) in (c, c)
                | Self -> getSome v#callingClass, (if v#obj <> None then (getSome v#obj)#objectClass else getSome v#staticClass)
                | Parent -> getSome (getSome v#callingClass)#parent, (if v#obj <> None then (getSome v#obj)#objectClass else getSome v#staticClass)
                | Static -> getSome v#staticClass, getSome v#staticClass
            in
            let m = if v#obj <> None && (getSome v#obj)#objectClass#instanceOf phpClass then
                try
                    Object.getClassMethod phpClass v#callingClass methodName (getSome v#obj)
                with
                    | Not_found -> Object.getClassStaticMethod phpClass v#callingClass methodName staticClass
            else
                Object.getClassStaticMethod phpClass v#callingClass methodName staticClass
            in m (List.map (fun f -> match self#eval v f with `Array a -> `Array (a#copy ()) | a -> a) argValues)
        end
        | ArrayConstructor l -> let phpArray = new PhpArray.phpArray in
            let addElement (e1, e2) = match self#eval v e1 with
                | `Null -> phpArray#offsetSet None (self#eval v e2)
                | o -> let `String offset = to_string o in phpArray#offsetSet (Some offset) (self#eval v e2)
            in
            List.iter addElement l;
            `Array phpArray
        | NewObject (className, argValues) -> `Object ((classRegistry#get (v#resolveNamespace className))#newObject (List.map (fun f -> match self#eval v f with `Array a -> `Array (a#copy ()) | a -> a) argValues))
        | Assign (a, f) -> let value = match self#eval v f with
            | `Array arr -> `Array (arr#copy ())
            | value -> value
            in
            begin try
                let var = self#eval_assignable_var v a in
                var#set value
            with
            | Not_found ->
                let var = object
                    val mutable v = value
                    method get = v
                    method set nv = v <- nv
                    end
                in self#assign_var v a var
            end;
            value
        | AssignByRef (a, b) ->
            let var = self#eval_assignable_var v b in
            self#assign_var v a var;
            var#get
        | BinaryAssign (op, a, f) -> self#eval v (Assign (a, BinaryOperation (op, Assignable a, f)))
        | PreInc a -> self#eval v (Assign (a, BinaryOperation (Plus, Assignable a, ConstValue (`Long 1))))
        | PostInc a -> let ret = self#eval v (Assignable a) in let _ = self#eval v (Assign (a, BinaryOperation (Plus, Assignable a, ConstValue (`Long 1)))) in ret
        | PreDec a -> self#eval v (Assign (a, BinaryOperation (Minus, Assignable a, ConstValue (`Long 1))))
        | PostDec a -> let ret = self#eval v (Assignable a) in let _ = self#eval v (Assign (a, BinaryOperation (Minus, Assignable a, ConstValue (`Long 1)))) in ret
        | Include (filename, required, once) -> let `String f = to_string (self#eval v filename) in fileRegistry#includeFile f required once (execFile v)

    method private assign_var v a var =
        match a with
        | Variable s -> v#vars#replace s var
        | VariableVariable e -> let `String s = to_string (self#eval v e) in v#vars#replace s var
        | ArrayOffset (e, o) ->
            let arr = match self#eval v e with `Array arr -> arr | _ -> raise BadType in
            begin match o with
                | None -> arr#offsetVarSet (arr#nextOffset) var
                | Some o -> let `String offset = to_string (self#eval v o) in arr#offsetVarSet offset var
            end
        | StaticProperty (classRef, propName) -> begin
            let phpClass = match classRef with
                | ClassName className -> classRegistry#get (v#resolveNamespace className)
                | Self -> getSome v#callingClass
                | Parent -> getSome (getSome v#callingClass)#parent
                | Static -> getSome v#staticClass
            in
            Object.setClassStaticPropertyVar phpClass v#callingClass propName var
            end
        | Property (obj, propName) -> begin match self#eval v obj with
            | `Object o -> Object.setObjectPropertyVar o v#callingClass propName var
            | _ -> raise BadType
        end
    method eval_assignable v a = (self#eval_assignable_var v a)#get
        
    method private eval_assignable_var v a =
        match a with
        | Variable s -> v#vars#find s
        | VariableVariable e -> let `String s = to_string (self#eval v e) in v#vars#find s
        | ArrayOffset (e, o) -> begin
            match o with
                | None -> raise MissingArrayOffset
                | Some o -> match self#eval v e with
                    | `Array a -> let `String offset = to_string (self#eval v o) in a#offsetVar offset
                    | _ -> raise BadType
        end
        | StaticProperty (classRef, propName) -> begin
            let phpClass = match classRef with
                | ClassName className -> classRegistry#get (v#resolveNamespace className)
                | Self -> getSome v#callingClass
                | Parent -> getSome (getSome v#callingClass)#parent
                | Static -> getSome v#staticClass
            in
            Object.getClassStaticPropertyVar phpClass v#callingClass propName
        end
        | Property (obj, propName) -> begin match self#eval v obj with
            | `Object o -> Object.getObjectPropertyVar o v#callingClass propName
            | _ -> raise BadType
        end
    end
