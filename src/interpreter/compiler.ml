open Language.Ast
open Language.Typing

exception NotTraversable

type 'v exec_return =
    | NoOp
    | Return of 'v
    | Break of int
    | Continue of int

class type ['v] variableRegistry
= object
    method replace : string -> <get : 'v; set : 'v -> unit > -> unit
    method find : string -> <get : 'v; set : 'v -> unit >
    method newScope : unit -> 'v variableRegistry
    method addFromParent : string -> unit
    method addFromGlobal : string -> unit
end

class type ['a, 'o, 'c] evalContext
= object
    method vars : ('a, 'o) value variableRegistry
    method obj : 'o option
    method callingClass : 'c option
    method staticClass : 'c option
    method namespace : string list
    method resolveNamespace : ?fallbackTest:(string -> bool) -> Language.Ast.name -> string
    method functions : ('a, 'o) value Registry.functionRegistry
    method classes : 'c Registry.classRegistry
    method files : ('a, 'o) value Registry.fileRegistry
    method functionScope : ?callingClass:'c -> ?obj:'o -> ?staticClass:'c -> unit -> ('a, 'o, 'c) evalContext
    method namespaceScope : namespace:(string list) -> namespaceUses:((string list * string option) list) -> unit -> ('a, 'o, 'c) evalContext
    method newClosure : ((('a, 'o) value as 'v) list -> 'v) -> 'v
    method setClosureFactory : (((('a, 'o) value as 'v) list -> 'v) -> 'v) -> unit
end

type ('a, 'o, 'c) compileClassContent =
    | ClassConstant of (('a, 'o, 'c) evalContext -> string * ('a, 'o) value)
    | ClassProperty of (('a, 'o, 'c) evalContext -> string * bool * visibility * ('a, 'o) value)
    | ClassMethod of (('a, 'o, 'c) evalContext -> string * visibility * ('c -> 'o -> ('a, 'o) value list -> ('a, 'o) value))
    | ClassStaticMethod of (('a, 'o, 'c) evalContext -> string * visibility * ('c -> 'c -> ('a, 'o) value list -> ('a, 'o) value))
    | ClassAbstractMethod of (string * bool * visibility * string list)

let functionDef name argNames compiledCode =
    fun (context:(_,_,_) evalContext) ->
        let f argValues =
            let localContext = context#functionScope () in
            List.iter2 (fun name value -> (localContext#vars#find name)#set value) argNames argValues;
            match compiledCode localContext with
                | Return v -> v
                | _ -> `Null
        in context#functions#add name f; NoOp

let classDef className isStatic isAbstract isFinal isInterface parentName implementsNames constants properties methods staticMethods abstractMethods =
    fun context ->
        let className = String.concat "\\" (context#namespace @ [className]) in
        let parent = match parentName with None -> None | Some n -> Some (context#classes#get n) in
        let implements = List.map (context#classes#get) implementsNames in
        context#classes#add
            className
            (new Object.phpClass
                className isStatic isAbstract isFinal isInterface parent implements
                (List.map (fun c -> c context) constants)
                (List.map (fun c -> c context) properties)
                (List.map (fun c -> c context) methods)
                (List.map (fun c -> c context) staticMethods)
                abstractMethods
            );
        NoOp
let is_break op = match op with Break _ -> true | _ -> false
let getSome o = match o with
    | None -> assert false
    | Some a -> a

class compiler
    = object (self)
    method compileStmtList sl =
        let cl = List.map (self#compileStmt) sl in
        let rec exec_list cl context =
            match cl with
            | [] -> NoOp
            | a::t -> match a context with
                | NoOp -> exec_list t context
                | r -> r
        in exec_list cl
    method compileStmt s =
        match s with
        | IgnoreResult e -> let ce = self#compileExpr e in fun context -> let _ = ce context in NoOp
        | Language.Ast.Return e -> let ce = self#compileExpr e in fun context -> Return (ce context)
        | Language.Ast.Break i -> fun context -> Break i
        | Language.Ast.Continue i -> fun context -> Continue i
        | FunctionDef (name, argNames, code) ->
            let compiledCode = self#compileStmtList code in
            functionDef name argNames compiledCode
        | ClassDef (className, isStatic, isAbstract, isFinal, isInterface, parentName, implementsNames, contents) ->
            let constants, properties, methods, staticMethods, abstractMethods =
                self#compileClassContentList contents
            in
            classDef className isStatic isAbstract isFinal isInterface parentName implementsNames constants properties methods staticMethods abstractMethods
        | Global name -> fun context -> context#vars#addFromGlobal name; NoOp
        | Echo e ->
            let ce = self#compileExpr e in
            fun context ->
                let `String s = to_string (ce context) in
                print_string s;
                NoOp
        | If (e, sl) ->
            let ce = self#compileExpr e in
            let compiledCode = self#compileStmtList sl in
            fun context ->
                let `Bool cond = to_bool (ce context) in
                if cond then compiledCode context else NoOp
        | IfElse (e, sl1, sl2) ->
            let ce = self#compileExpr e in
            let compiledCode1 = self#compileStmtList sl1 in
            let compiledCode2 = self#compileStmtList sl2 in
            fun context ->
                let `Bool cond = to_bool (ce context) in
                if cond then compiledCode1 context else compiledCode2 context
        | While (e, sl) -> begin
            let ce = self#compileExpr e in
            let compiledCode = self#compileStmtList sl in
            fun context ->
                let result = ref NoOp in
                while not (is_break !result) && let `Bool cond = to_bool (ce context) in cond do
                    result := compiledCode context
                done;
                match !result with
                    | Break i when i <= 1 -> NoOp
                    | Continue i when i <= 1 -> NoOp
                    | Break i -> Break (i-1)
                    | Continue i -> Continue (i-1)
                    | _ -> !result
            end
        | For (eInit, eEnd, eLoop, sl) -> begin
            let ceInit = List.map self#compileExpr eInit in
            let ceEnd = List.map self#compileExpr eEnd in
            let ceLoop = List.map self#compileExpr eLoop in
            let compiledCode = self#compileStmtList sl in
            fun context ->
                let result = ref NoOp in
                let rec eval_all es = match es with
                    | [] -> `Bool true
                    | [ce] -> to_bool (ce context)
                    | ce::l -> let _ = ce context in eval_all l
                in
                let _ = eval_all ceInit in
                while not (is_break !result) && let `Bool cond = eval_all ceEnd in cond do
                    result := compiledCode context;
                    if not (is_break !result) then
                        let _ = eval_all ceLoop in ()
                    else ()
                done;
                match !result with
                    | Break i when i <= 1 -> NoOp
                    | Continue i when i <= 1 -> NoOp
                    | Break i -> Break (i-1)
                    | Continue i -> Continue (i-1)
                    | _ -> !result
            end
        | Foreach (e, ko, vn, sl) -> begin
            let ce = self#compileExpr e in
            let compiledCode = self#compileStmtList sl in
            fun context ->
                match ce context with
                | `Array a -> begin
                    a#rewind ();
                    let result = ref NoOp in
                    while not (is_break !result) && a#valid() do
                        ((context#vars)#find vn)#set (a#current ());
                        begin match ko with
                            | None -> ()
                            | Some kn -> let k = a#key () in ((context#vars)#find kn)#set (`String k)
                        end;
                        result := compiledCode context;
                        if not (is_break !result) then
                            a#next ()
                        else ()
                    done;
                    match !result with
                        | Break i when i <= 1 -> NoOp
                        | Continue i when i <= 1 -> NoOp
                        | Break i -> Break (i-1)
                        | Continue i -> Continue (i-1)
                        | _ -> !result
                end
                | _ -> raise NotTraversable
            end
    
    method compileClassContentList l =
        match l with
        | [] -> ([], [], [], [], [])
        | a::t ->
            let c, p, m, sm, am = self#compileClassContentList t in
            match self#compileClassContent a with
            | ClassConstant cc -> cc::c, p, m, sm, am
            | ClassProperty cp -> c, cp::p, m, sm, am
            | ClassMethod cm -> c, p, cm::m, sm, am
            | ClassStaticMethod csm -> c, p, m, csm::sm, am
            | ClassAbstractMethod cam -> c, p, m, sm, cam::am
    method compileClassContent c =
        match c with
        | ConstantDef (name, e) ->
            let ce = self#compileExpr e in
            ClassConstant (fun context -> (name, ce context))
        | PropertyDef (name, isStatic, visibility, init) ->
            let inited =
                match init with
                | None -> fun context -> `Null
                | Some i -> self#compileExpr i
            in ClassProperty (fun context -> (name, isStatic, visibility, inited context))
        | MethodDef (name, isStatic, visibility, argNames, code) ->
            begin match isStatic with
            | true ->
                let compiledCode = self#compileStmtList code in
                ClassStaticMethod (fun context ->
                    let f inClass finalClass argValues =
                    let localContext = context#functionScope ~callingClass:inClass ~staticClass:finalClass () in
                    List.iter2 (fun name value -> (localContext#vars#find name)#set value) argNames argValues;
                    match compiledCode localContext with
                        | Return v -> v
                        | _ -> `Null
                    in
                    (name, visibility, f)
                )
            | false ->
                let compiledCode = self#compileStmtList code in
                ClassMethod (fun context ->
                    let f inClass obj argValues =
                    let localContext = context#functionScope ~callingClass:inClass ~obj () in
                    List.iter2 (fun name value -> (localContext#vars#find name)#set value) argNames argValues;
                    match compiledCode localContext with
                        | Return v -> v
                        | _ -> `Null
                    in
                    (name, visibility, f)
                )
            end
        | AbstractMethodDef (name, isStatic, visibility, argNames) ->
            ClassAbstractMethod (name, isStatic, visibility, argNames)
    method compileFile l =
        let compiledNl = self#compileNamespaceList l in
        fun context ->
            match compiledNl context with
            | Return v -> v
            | _ -> `Bool true
    method compileNamespace n =
        match n with
        | NamespaceBlock (nname, uses, l) ->
            let compiledCode = self#compileStmtList l in
            fun (context:(_,_,_) evalContext) ->
                let newContext = context#namespaceScope ~namespace:nname ~namespaceUses:uses () in
                compiledCode newContext
    method compileNamespaceList nl =
        let cnl = List.map self#compileNamespace nl in
        let rec execNamespaceList cnl context =
            match cnl with
            | [] -> NoOp
            | a::t -> match a context with
                | NoOp -> execNamespaceList t context
                | r -> r
        in execNamespaceList cnl
    
    method compileExpr e =
        match e with
        | ConstValue f -> fun _ -> Expression.convertConst f
        | ConcatList l ->
            let compiledExprs = List.map self#compileExpr l in
            fun context -> `String (String.concat "" (List.map (fun ce -> let `String s = to_string (ce context) in s) compiledExprs))
        | Assignable a ->
            let ca = self#compileAssignable a in
            fun context -> (ca context)#get
        | This -> fun context -> `Object (getSome context#obj)
        | BinaryOperation (op, f, g) ->
            let cf = self#compileExpr f in
            let cg = self#compileExpr g in
            begin match op with
            | Plus -> fun context -> Expression.eval_binary (+) (+.) (cf context) (cg context)
            | Minus -> fun context -> Expression.eval_binary (-) (-.) (cf context) (cg context)
            | Mult -> fun context -> Expression.eval_binary ( * ) ( *. ) (cf context) (cg context)
            | Div -> fun context -> Expression.eval_div (cf context) (cg context)
            | Modulo -> fun context -> Expression.eval_binary (mod) (mod_float) (cf context) (cg context)
            | Concat -> fun context -> let `String s1 = to_string (cf context) and `String s2 = to_string (cg context) in `String (s1 ^ s2)
            | BitwiseAnd -> fun context -> Expression.bitwise_operator (land) (cf context) (cg context)
            | BitwiseOr -> fun context -> Expression.bitwise_operator (lor) (cf context) (cg context)
            | BitwiseXor -> fun context -> Expression.bitwise_operator (lxor) (cf context) (cg context)
            | ShiftLeft -> fun context -> Expression.bitwise_operator (lsl) (cf context) (cg context)
            | ShiftRight -> fun context -> Expression.bitwise_operator (lsr) (cf context) (cg context)
            end
        | UnaryMinus f ->
            let cf = self#compileExpr f in
            fun context -> Expression.eval_binary (-) (-.) (`Long 0) (cf context)
        | And (f, g) ->
            let cf = self#compileExpr f in
            let cg = self#compileExpr g in
            fun context -> Expression.boolean_operator (&&) (cf context) (cg context)
        | Or (f, g) ->
            let cf = self#compileExpr f in
            let cg = self#compileExpr g in
            fun context -> Expression.boolean_operator (||) (cf context) (cg context)
        | Xor (f, g) ->
            let cf = self#compileExpr f in
            let cg = self#compileExpr g in
            fun context -> Expression.boolean_operator (<>) (cf context) (cg context)
        | Not f ->
            let cf = self#compileExpr f in
            fun context -> let `Bool b = to_bool (cf context) in `Bool (not b)
        | Comparison (op, f, g) ->
            let cf = self#compileExpr f in
            let cg = self#compileExpr g in
            fun context -> `Bool (Expression.compare_all op (cf context) (cg context))
        | Closure (argNames, uses, code) ->
            let compiledCode = self#compileStmtList code in
            fun context ->
                let f argValues =
                    let localContext = context#functionScope () in
                    List.iter (fun name -> localContext#vars#addFromParent name) uses;
                    List.iter2 (fun name value -> (localContext#vars#find name)#set value) argNames argValues;
                    match compiledCode localContext with
                        | Return v -> v
                        | _ -> `Null
                in context#newClosure f
        | FunctionCall (name, argValues) ->
            let compiledArgs = List.map self#compileExpr argValues in
            fun context -> context#functions#exec
                (context#resolveNamespace ~fallbackTest:(context#functions#has) name)
                (List.map (fun cf -> match cf context with `Array a -> `Array (a#copy ()) | a -> a) compiledArgs)
        | Invoke (e, argValues) ->
            let ce = self#compileExpr e in
            let compiledArgs = List.map self#compileExpr argValues in
            fun context ->
                begin match ce context with
                | `Object o -> (o#getObjectMethod None "__invoke") (List.map (fun cf -> match cf context with `Array a -> `Array (a#copy ()) | a -> a) compiledArgs)
                | _ -> failwith "not a closure"
                end
        | Language.Ast.ClassConstant (classRef, constantName) ->
            fun context -> let phpClass = match classRef with
                | ClassName className -> context#classes#get (context#resolveNamespace className)
                | Self -> getSome context#callingClass
                | Parent -> getSome (getSome context#callingClass)#parent
                | Static -> getSome context#staticClass
            in phpClass#getClassConstant constantName
        | MethodCall (obj, methodName, argValues) ->
            let cobj = self#compileExpr obj in
            let compiledArgs = List.map self#compileExpr argValues in
            fun context -> begin match cobj context with
                | `Object o -> o#getObjectMethod context#callingClass methodName (List.map (fun cf -> match cf context with `Array a -> `Array (a#copy ()) | a -> a) compiledArgs)
                | _ -> raise BadType
            end
        | StaticMethodCall (classRef, methodName, argValues) -> begin
            let compiledArgs = List.map self#compileExpr argValues in
            fun context ->
                let (phpClass, staticClass) = match classRef with
                    | ClassName className -> let c = context#classes#get (context#resolveNamespace className) in (c, c)
                    | Self -> getSome context#callingClass, (if context#obj <> None then (getSome context#obj)#objectClass else getSome context#staticClass)
                    | Parent -> getSome (getSome context#callingClass)#parent, (if context#obj <> None then (getSome context#obj)#objectClass else getSome context#staticClass)
                    | Static -> getSome context#staticClass, getSome context#staticClass
                in
                let m = if context#obj <> None && (getSome context#obj)#objectClass#instanceOf phpClass then
                    try
                        phpClass#getClassMethod context#callingClass methodName (getSome context#obj)
                    with
                        | Not_found -> phpClass#getClassStaticMethod context#callingClass methodName staticClass
                else
                    phpClass#getClassStaticMethod context#callingClass methodName staticClass
                in m (List.map (fun cf -> match cf context with `Array a -> `Array (a#copy ()) | a -> a) compiledArgs)
            end
        | ArrayConstructor l ->
            let compiledL = List.map (fun (e1, e2) -> ((match e1 with None -> None | Some e1 -> Some (self#compileExpr e1)), self#compileExpr e2)) l in
            fun context ->
                let phpArray = new PhpArray.phpArray in
                let addElement (e1, e2) = match e1 with
                    | None -> phpArray#offsetSet None (e2 context)
                    | Some o -> let `String offset = to_string (o context) in phpArray#offsetSet (Some offset) (e2 context)
                in
                List.iter addElement compiledL;
                `Array phpArray
        | NewObject (className, argValues) ->
            let compiledArgs = List.map self#compileExpr argValues in
            fun context -> `Object ((context#classes#get (context#resolveNamespace className))#newObject (List.map (fun cf -> match cf context with `Array a -> `Array (a#copy ()) | a -> a) compiledArgs))
        | Assign (a, f) ->
            let ca = self#compileAssignable a in
            let cav = self#compileAssignVar a in
            let cf = self#compileExpr f in
            fun context ->
                let value = match cf context with
                | `Array arr -> `Array (arr#copy ())
                | value -> value
                in
                begin try
                    let var = ca context in
                    var#set value
                with
                | Not_found ->
                    let var = object
                        val mutable v = value
                        method get = v
                        method set nv = v <- nv
                        end
                    in cav context var
                end;
                value
        | AssignByRef (a, b) ->
            let cav = self#compileAssignVar a in
            let cb = self#compileAssignable b in
            fun context ->
                let var = cb context in
                cav context var;
                var#get
        | BinaryAssign (op, a, f) -> self#compileExpr (Assign (a, BinaryOperation (op, Assignable a, f)))
        | PreInc a -> self#compileExpr (Assign (a, BinaryOperation (Plus, Assignable a, ConstValue (`Long 1))))
        | PostInc a ->
            let ret = self#compileExpr (Assignable a) in
            let inc = self#compileExpr (Assign (a, BinaryOperation (Plus, Assignable a, ConstValue (`Long 1)))) in
            fun context -> let _ = inc context in ret context
        | PreDec a -> self#compileExpr (Assign (a, BinaryOperation (Minus, Assignable a, ConstValue (`Long 1))))
        | PostDec a ->
            let ret = self#compileExpr (Assignable a) in
            let inc = self#compileExpr (Assign (a, BinaryOperation (Minus, Assignable a, ConstValue (`Long 1)))) in
            fun context -> let _ = inc context in ret context
        | Include (filename, required, once) ->
            let cf = self#compileExpr filename in
            fun context -> let `String f = to_string (cf context) in context#files#includeFile f required once (fun l -> (self#compileFile l) context)

    method private compileAssignable a =
        match a with
        | Variable s -> fun context -> context#vars#find s
        | VariableVariable e ->
            let ce = self#compileExpr e in
            fun context -> let `String s = to_string (ce context) in context#vars#find s
        | ArrayOffset (e, o) ->
            let ce = self#compileExpr e in
            let co = match o with None -> None | Some o -> Some (self#compileExpr o) in
            fun context ->
                begin match co with
                | None -> raise Expression.MissingArrayOffset
                | Some co -> match ce context with
                    | `Array a -> let `String offset = to_string (co context) in a#offsetVar offset
                    | _ -> raise BadType
                end
        | StaticProperty (classRef, propName) ->
            fun context ->
                let phpClass =
                    match classRef with
                    | ClassName className -> context#classes#get (context#resolveNamespace className)
                    | Self -> getSome context#callingClass
                    | Parent -> getSome (getSome context#callingClass)#parent
                    | Static -> getSome context#staticClass
                in
                phpClass#getClassStaticPropertyVar context#callingClass propName
        | Property (obj, propName) ->
            let cobj = self#compileExpr obj in
            fun context ->
                begin match cobj context with
                | `Object o -> o#getObjectPropertyVar context#callingClass propName
                | _ -> raise BadType
                end
    method private compileAssignVar a =
        match a with
        | Variable s -> fun context var -> context#vars#replace s var
        | VariableVariable e ->
            let ce = self#compileExpr e in
            fun context var -> let `String s = to_string (ce context) in context#vars#replace s var
        | ArrayOffset (e, o) ->
            let ce = self#compileExpr e in
            let co = match o with None -> None | Some o -> Some (self#compileExpr o) in
            fun context var ->
                let arr = match ce context with `Array arr -> arr | _ -> raise BadType in
                begin match co with
                | None -> arr#offsetVarSet (arr#nextOffset) var
                | Some co -> let `String offset = to_string (co context) in arr#offsetVarSet offset var
                end
        | StaticProperty (classRef, propName) ->
            fun context var ->
                let phpClass =
                    match classRef with
                    | ClassName className -> context#classes#get (context#resolveNamespace className)
                    | Self -> getSome context#callingClass
                    | Parent -> getSome (getSome context#callingClass)#parent
                    | Static -> getSome context#staticClass
                in
                phpClass#setClassStaticPropertyVar context#callingClass propName var
        | Property (obj, propName) ->
            let cobj = self#compileExpr obj in
            fun context var ->
                begin match cobj context with
                | `Object o -> o#setObjectPropertyVar context#callingClass propName var
                | _ -> raise BadType
                end
end

