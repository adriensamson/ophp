open Language.Ast
open Language.Typing

exception NotTraversable

class ['v] variable (value : 'v) =
    object
    val mutable v = value
    method get = v
    method set nv = v <- nv
    end

type 'v exec_return =
    | NoOp
    | Return of 'v variable
    | Break of int
    | Continue of int
    | Exception of 'v

class type ['v] variableRegistry
= object
    method replace : string -> 'v variable -> unit
    method find : string -> 'v variable
    method replaceSuperglobal : string -> 'v variable -> unit
    method findSuperglobal : string -> 'v variable
    method newScope : string -> 'v variableRegistry
    method addFromParent : ?byRef:bool -> string -> unit
    method addFromGlobal : string -> unit
    method addFromStatic : string -> 'v -> unit
end

class type ['a, 'o, 'c] evalContext
= object
    method vars : ('a, 'o) value variableRegistry
    method obj : 'o option
    method callingClass : 'c option
    method staticClass : 'c option
    method namespace : string list
    method resolveNamespace : ?fallbackTest:(string -> bool) -> Language.Ast.name -> string
    method constants : ('a, 'o) value Registry.constantRegistry
    method functions :('a, 'o) value variable Registry.functionRegistry
    method classes : 'c Registry.classRegistry
    method files : ('a, 'o) value Registry.fileRegistry
    method functionScope : ?callingClass:'c -> ?obj:'o -> ?staticClass:'c -> string -> ('a, 'o, 'c) evalContext
    method namespaceScope : namespace:(string list) -> namespaceUses:((string list * string option) list) -> unit -> ('a, 'o, 'c) evalContext
    method newClosure : ((('a, 'o) value as 'v) variable list -> 'v variable) -> 'v
    method setClosureFactory : (((('a, 'o) value as 'v) variable list -> 'v variable) -> 'v) -> unit
end

class compileContext = object
    val file = ""
    val dir = ""
    val func = ""
    val cla = ""
    val trait = ""
    val meth = ""
    val namespace = ""
    method getFile = file
    method getDir = dir
    method getFunction = func
    method getClass = cla
    method getTrait = trait
    method getMethod = meth
    method getNamespace = namespace
    
    method setFile f =
        let absF = FilePath.make_absolute (Sys.getcwd ()) f in
        let absD = FilePath.dirname absF in
        {< file = absF; dir = absD; namespace = "" >}
    method setFunction f = {< func = f >}
    method setClass c = {< cla = c >}
    method setTrait t = {< trait = t >}
    method setMethod m = {< func = m; meth = cla ^ "::" ^ m >}
    method setNamespace n = {< namespace = n >}
end

type ('a, 'o, 'c) compileClassContent =
    | ClassConstant of (('a, 'o, 'c) evalContext -> string * ('a, 'o) value)
    | ClassProperty of (('a, 'o, 'c) evalContext -> string * bool * visibility * ('a, 'o) value)
    | ClassMethod of (('a, 'o, 'c) evalContext -> string * visibility * ('c -> 'o -> ('a, 'o) value variable list -> ('a, 'o) value variable))
    | ClassStaticMethod of (('a, 'o, 'c) evalContext -> string * visibility * ('c -> 'c -> ('a, 'o) value variable list -> ('a, 'o) value variable))
    | ClassAbstractMethod of (string * bool * bool * visibility * (string * bool * typeHint) list)

let checkTypeHint (localContext : (_,_,_) evalContext) typeHint value = match typeHint, value with
    | NoTypeHint, _
    | ArrayTypeHint, `Array _ -> ()
    | ClassTypeHint name, `Object o when o#instanceOf (localContext#classes#get (localContext#resolveNamespace name)) -> ()
    | _, _ -> failwith "Bad Type hint"

let assignParam localContext (name, byRef, typeHint) var =
    if byRef then begin
        checkTypeHint localContext typeHint var#get;
        localContext#vars#replace name var
    end else begin
        let v = match var#get with
            | `Array a -> `Array (a#copy ())
            | a -> a
        in
        checkTypeHint localContext typeHint v;
        (localContext#vars#find name)#set v
    end

let rec assignParams localContext argConfs vars =
    match argConfs, vars with
    | (n, r, t, _)::acs, v::vs -> assignParam localContext (n, r, t) v; assignParams localContext acs vs
    | [], _ -> ()
    | (n, r, t, Some d)::acs, [] -> assignParam localContext (n, r, t) (new variable (d localContext)); assignParams localContext acs []
    | (_, _, _, None)::acs, [] -> failwith "Missing arguments"

let makeFunction localContext returnByRef argConf compiledCode argVars =
    assignParams localContext argConf argVars;
    match compiledCode localContext with
        | Return v -> if returnByRef then v else new variable v#get
        | _ -> new variable `Null

let functionDef name returnByRef argConf compiledCode =
    fun (context:(_,_,_) evalContext) ->
        let f argVars =
            let localContext = context#functionScope name in
            makeFunction localContext returnByRef argConf compiledCode argVars
        in context#functions#add name f; NoOp

let classDef className isStatic isAbstract isFinal isInterface parentName implementsNames constants properties methods staticMethods abstractMethods =
    fun context ->
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

let rec fold_until f g l acc = match l with
    | [] -> acc
    | a::s ->
        let result = f a acc in
        if g result then
            result
        else
            fold_until f g s result

let invoke_callable v context args = match v with
    | `Object o -> (o#getObjectMethod None "__invoke") args
    | `String s -> context#functions#exec s args
    | `Array a -> begin
        (*try*) match (a#get "0")#get, (a#get "1")#get with
            | `String cn, `String fn -> let cl = context#classes#get cn in (cl#getClassStaticMethod None fn cl) args
            | `Object o, `String fn -> (o#getObjectMethod None fn) args
            | _, _ -> failwith "not a callable array"
        (*with
        | _ -> failwith "not an callable array"*)
        end
    | _ -> failwith "not a callable"

class compiler
    = object (self)
    method compileStmtList (compileContext : compileContext) sl =
        let cl = List.map (self#compileStmt compileContext) sl in
        let rec exec_list cl context =
            match cl with
            | [] -> NoOp
            | a::t -> match a context with
                | NoOp -> exec_list t context
                | r -> r
        in exec_list cl
    method compileStmt compileContext s =
        match s with
        | IgnoreResult e -> let ce = self#compileExpr compileContext e in fun context -> let _ = ce context in NoOp
        | Language.Ast.Return e -> let ce = self#compileExprVar compileContext e in fun context -> Return (ce context)
        | Language.Ast.Break i -> fun context -> Break i
        | Language.Ast.Continue i -> fun context -> Continue i
        | FunctionDef (name, returnByRef, argConf, code) ->
            let fullFunctionName =
                if compileContext#getNamespace = "" then
                    name
                else
                    compileContext#getNamespace ^ "\\" ^ name
            in
            let compiledCode = self#compileStmtList (compileContext#setFunction fullFunctionName) code in
            let compiledArgConf = List.map (self#compileArgConf compileContext) argConf in
            functionDef name returnByRef compiledArgConf compiledCode
        | ClassDef (className, isStatic, isAbstract, isFinal, isInterface, parentName, implementsNames, contents) ->
            let fullClassName =
                if compileContext#getNamespace = "" then
                    className
                else
                    compileContext#getNamespace ^ "\\" ^ className
            in
            let constants, properties, methods, staticMethods, abstractMethods =
                self#compileClassContentList (compileContext#setClass fullClassName) contents
            in
            classDef fullClassName isStatic isAbstract isFinal isInterface parentName implementsNames constants properties methods staticMethods abstractMethods
        | Global name -> fun context -> context#vars#addFromGlobal name; NoOp
        | StaticVar (name, e) ->
            let ce = self#compileExpr compileContext e in
            fun context -> context#vars#addFromStatic name (ce context); NoOp
        | Echo e ->
            let ce = self#compileExpr compileContext e in
            fun context ->
                let `String s = to_string (ce context) in
                print_string s;
                NoOp
        | If (e, sl) ->
            let ce = self#compileExpr compileContext e in
            let compiledCode = self#compileStmtList compileContext sl in
            fun context ->
                let `Bool cond = to_bool (ce context) in
                if cond then compiledCode context else NoOp
        | IfElse (e, sl1, sl2) ->
            let ce = self#compileExpr compileContext e in
            let compiledCode1 = self#compileStmtList compileContext sl1 in
            let compiledCode2 = self#compileStmtList compileContext sl2 in
            fun context ->
                let `Bool cond = to_bool (ce context) in
                if cond then compiledCode1 context else compiledCode2 context
        | While (e, sl) -> begin
            let ce = self#compileExpr compileContext e in
            let compiledCode = self#compileStmtList compileContext sl in
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
            let ceInit = List.map (self#compileExpr compileContext) eInit in
            let ceEnd = List.map (self#compileExpr compileContext) eEnd in
            let ceLoop = List.map (self#compileExpr compileContext) eLoop in
            let compiledCode = self#compileStmtList compileContext sl in
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
            let ce = self#compileExpr compileContext e in
            let compiledCode = self#compileStmtList compileContext sl in
            fun context ->
                match ce context with
                | `Array a -> begin
                    a#rewind ();
                    let result = ref NoOp in
                    while not (is_break !result) && a#valid() do
                        ((context#vars)#find vn)#set (a#current ())#get;
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
        | Switch (e, cl) -> begin
            let ce = self#compileExpr compileContext e in
            let ccl = List.map (fun (e, sl) -> ((match e with None -> None | Some e -> Some (self#compileExpr compileContext e)), self#compileStmtList compileContext sl)) cl in
            fun context ->
                let v = ce context in
                let result = ref NoOp in
                let cases = ref ccl in
                while not (is_break !result) && !cases <> [] do
                    let ce, csl = List.hd !cases in
                    begin if match ce with None -> true | Some ce -> Expression.compare_values v (ce context) = Some 0 then
                        result := csl context
                    end;
                    cases := List.tl !cases
                done;
                match !result with
                | Break i when i <= 1 -> NoOp
                | Break i -> Break (i-1)
                | _ -> !result
            end
        | Throw e ->
            let ce = self#compileExpr compileContext e in
            fun context ->
                begin match ce context with
                | `Object o as v when o#instanceOf (context#classes#get "Exception") -> Exception v
                | _ -> failwith "must throw an exception"
                end
        | TryCatch (code, catches) ->
            let compiledCode = self#compileStmtList compileContext code in
            let compiledCatches = List.map (fun (cn, vn, code) -> (cn, vn, self#compileStmtList compileContext code)) catches in
            fun context ->
                match compiledCode context with
                | Exception (`Object o) as e -> begin
                    let f = fun (cn, vn, ccode) (e, catched) ->
                        if o#instanceOf (context#classes#get (context#resolveNamespace cn)) then
                            ((context#vars#find vn)#set (`Object o); ccode context, true)
                        else
                            (e, false)
                    in
                    let r, _ = fold_until f (fun (_, b) -> b) compiledCatches (e, false) in
                    r
                    end
                | r -> r
    method compileClassContentList compileContext l =
        match l with
        | [] -> ([], [], [], [], [])
        | a::t ->
            let c, p, m, sm, am = self#compileClassContentList compileContext t in
            match self#compileClassContent compileContext a with
            | ClassConstant cc -> cc::c, p, m, sm, am
            | ClassProperty cp -> c, cp::p, m, sm, am
            | ClassMethod cm -> c, p, cm::m, sm, am
            | ClassStaticMethod csm -> c, p, m, csm::sm, am
            | ClassAbstractMethod cam -> c, p, m, sm, cam::am
    method compileClassContent compileContext c =
        match c with
        | ConstantDef (name, e) ->
            let ce = self#compileExpr compileContext e in
            ClassConstant (fun context -> (name, ce context))
        | PropertyDef (name, isStatic, visibility, init) ->
            let inited =
                match init with
                | None -> fun context -> `Null
                | Some i -> self#compileExpr compileContext i
            in ClassProperty (fun context -> (name, isStatic, visibility, inited context))
        | MethodDef (name, isStatic, isFinal, returnByRef, visibility, argConf, code) ->
            begin match isStatic with
            | true ->
                let compiledCode = self#compileStmtList (compileContext#setMethod name) code in
                let compiledArgConf = List.map (self#compileArgConf compileContext) argConf in
                ClassStaticMethod (fun context ->
                    let f inClass finalClass argVars =
                        let localContext = context#functionScope ~callingClass:inClass ~staticClass:finalClass compileContext#getMethod in
                        makeFunction localContext returnByRef compiledArgConf compiledCode argVars
                    in
                    (name, visibility, f)
                )
            | false ->
                let compiledCode = self#compileStmtList compileContext code in
                let compiledArgConf = List.map (self#compileArgConf compileContext) argConf in
                ClassMethod (fun context ->
                    let f inClass obj argVars =
                        let localContext = context#functionScope ~callingClass:inClass ~obj compileContext#getMethod in
                        makeFunction localContext returnByRef compiledArgConf compiledCode argVars
                    in
                    (name, visibility, f)
                )
            end
        | AbstractMethodDef (name, isStatic, returnByRef, visibility, argConf) ->
            ClassAbstractMethod (name, isStatic, returnByRef, visibility, List.map (fun (n, r, v, e) -> (n, r, v)) argConf)
    method compileFile compileContext l =
        let compiledNl = self#compileNamespaceList compileContext l in
        fun context ->
            match compiledNl context with
            | Return v -> v#get
            | _ -> `Bool true
    method compileNamespace compileContext n =
        match n with
        | NamespaceBlock (nname, uses, l) ->
            let compiledCode = self#compileStmtList (compileContext#setNamespace (String.concat "\\" nname)) l in
            fun (context:(_,_,_) evalContext) ->
                let newContext = context#namespaceScope ~namespace:nname ~namespaceUses:uses () in
                compiledCode newContext
    method compileNamespaceList compileContext nl =
        let cnl = List.map (self#compileNamespace compileContext) nl in
        let rec execNamespaceList cnl context =
            match cnl with
            | [] -> NoOp
            | a::t -> match a context with
                | NoOp -> execNamespaceList t context
                | r -> r
        in execNamespaceList cnl
    
    method compileExpr compileContext e =
        match e with
        | ConstValue f -> fun _ -> Expression.convertConst f
        | Constant name -> begin match name with
            | RelativeName ([], "__LINE__") -> fun _ -> `Long 0
            | RelativeName ([], "__FILE__") -> fun _ -> `String compileContext#getFile
            | RelativeName ([], "__DIR__") -> fun _ -> `String compileContext#getDir
            | RelativeName ([], "__FUNCTION__") -> fun _ -> `String compileContext#getFunction
            | RelativeName ([], "__CLASS__") -> fun _ -> `String compileContext#getClass
            | RelativeName ([], "__TRAIT__") -> fun _ -> `String compileContext#getTrait
            | RelativeName ([], "__METHOD__") -> fun _ -> `String compileContext#getMethod
            | RelativeName ([], "__NAMESPACE__") -> fun _ -> `String compileContext#getNamespace
            | _ -> fun (context: (_,_,_) evalContext) -> context#constants#get (context#resolveNamespace ~fallbackTest:(context#constants#has) name)
            end
        | Cast (ctype, e) ->
            let ce = self#compileExpr compileContext e in
            begin match ctype with
                | CastToLong -> fun context -> to_long (ce context)
                | CastToDouble -> fun context -> to_double (ce context)
                | CastToNull -> fun context -> ignore (ce context); `Null
                | CastToBool -> fun context -> to_bool (ce context)
                | CastToString -> fun context -> to_string (ce context)
                | CastToArray -> begin
                    fun context -> match (ce context) with
                        | `Array a -> `Array a
                        | `Object o -> failwith "object array cast not implemented"
                        | v -> let a = new PhpArray.phpArray in a#set "0" (new variable v); `Array a
                    end
                | CastToObject -> failwith "object cast not implemented"
            end
        | ConcatList l ->
            let compiledExprs = List.map (self#compileExpr compileContext) l in
            fun context -> `String (String.concat "" (List.map (fun ce -> let `String s = to_string (ce context) in s) compiledExprs))
        | Assignable _ ->
            let f = self#compileExprVar compileContext e in
            fun context -> (f context)#get
        | This -> fun context -> `Object (getSome context#obj)
        | BinaryOperation (op, f, g) ->
            let cf = self#compileExpr compileContext f in
            let cg = self#compileExpr compileContext g in
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
            let cf = self#compileExpr compileContext f in
            fun context -> Expression.eval_binary (-) (-.) (`Long 0) (cf context)
        | And (f, g) ->
            let cf = self#compileExpr compileContext f in
            let cg = self#compileExpr compileContext g in
            fun context ->
                let `Bool c = to_bool (cf context) in
                if c then
                    to_bool (cg context)
                else `Bool false
        | Or (f, g) ->
            let cf = self#compileExpr compileContext f in
            let cg = self#compileExpr compileContext g in
            fun context ->
                let `Bool c = to_bool (cf context) in
                if not c then
                    to_bool (cg context)
                else `Bool true
        | Xor (f, g) ->
            let cf = self#compileExpr compileContext f in
            let cg = self#compileExpr compileContext g in
            fun context -> Expression.boolean_operator (<>) (cf context) (cg context)
        | Not f ->
            let cf = self#compileExpr compileContext f in
            fun context -> let `Bool b = to_bool (cf context) in `Bool (not b)
        | Comparison (op, f, g) ->
            let cf = self#compileExpr compileContext f in
            let cg = self#compileExpr compileContext g in
            fun context -> `Bool (Expression.compare_all op (cf context) (cg context))
        | TertiaryOperator (e1, e2, e3) ->
            let ce1 = self#compileExpr compileContext e1 in
            let ce2 = self#compileExpr compileContext e2 in
            let ce3 = self#compileExpr compileContext e3 in
            fun context ->
                let `Bool cond = to_bool (ce1 context) in
                if cond then ce2 context else ce3 context
        | InstanceOf (e, className) ->
            let ce = self#compileExpr compileContext e in
            fun context -> begin
                match ce context with
                | `Object o -> `Bool (o#instanceOf (context#classes#get (context#resolveNamespace className)))
                | _ -> `Bool false
                end
        | Closure (returnByRef, argConf, uses, code) ->
            let compiledCode = self#compileStmtList compileContext code in
            let compiledArgConf = List.map (self#compileArgConf compileContext) argConf in
            fun context ->
                let f argVars =
                    let localContext = context#functionScope ("Closure:" ^ string_of_int (Oo.id (object end))) in
                    List.iter (fun (name, byRef) -> localContext#vars#addFromParent ~byRef name) uses;
                    makeFunction localContext returnByRef compiledArgConf compiledCode argVars
                in context#newClosure f
        | FunctionCall (_,_) ->
            let f = self#compileExprVar compileContext e in
            fun context -> (f context)#get
        | Invoke (_,_) ->
            let f = self#compileExprVar compileContext e in
            fun context -> (f context)#get
        | Language.Ast.ClassConstant (classRef, constantName) ->
            fun context -> let phpClass = match classRef with
                | ClassName className -> context#classes#get (context#resolveNamespace className)
                | Self -> getSome context#callingClass
                | Parent -> getSome (getSome context#callingClass)#parent
                | Static -> getSome context#staticClass
            in phpClass#getClassConstant constantName
        | MethodCall (_,_,_) ->
            let f = self#compileExprVar compileContext e in
            fun context -> (f context)#get
        | VariableMethodCall (_,_,_) ->
            let f = self#compileExprVar compileContext e in
            fun context -> (f context)#get
        | StaticMethodCall (_,_,_) ->
            let f = self#compileExprVar compileContext e in
            fun context -> (f context)#get
        | VariableStaticMethodCall (_,_,_) ->
            let f = self#compileExprVar compileContext e in
            fun context -> (f context)#get
        | ArrayConstructor l ->
            let compiledL = List.map (fun (e1, e2) -> ((match e1 with None -> None | Some e1 -> Some (self#compileExpr compileContext e1)), self#compileExpr compileContext e2)) l in
            fun context ->
                let phpArray = new PhpArray.phpArray in
                let addElement (e1, e2) = match e1 with
                    | None -> phpArray#setOption None (new variable (e2 context))
                    | Some o -> let `String offset = to_string (o context) in phpArray#setOption (Some offset) (new variable (e2 context))
                in
                List.iter addElement compiledL;
                `Array phpArray
        | NewObject (classRef, argValues) ->
            let compiledArgs = List.map (self#compileExprVar compileContext) argValues in
            fun context ->
                let phpClass = match classRef with
                    | ClassName className -> context#classes#get (context#resolveNamespace className)
                    | Self -> getSome context#callingClass
                    | Parent -> getSome (getSome context#callingClass)#parent
                    | Static -> getSome context#staticClass
                in
                `Object (phpClass#newObject (List.map (fun cf -> cf context) compiledArgs) context#callingClass)
        | VariableNewObject (e, argValues) ->
            let compiledArgs = List.map (self#compileExprVar compileContext) argValues in
            let ce = self#compileExpr compileContext e in
            fun context ->
                let `String cname = to_string (ce context) in
                `Object ((context#classes#get cname)#newObject (List.map (fun cf -> cf context) compiledArgs) context#callingClass)
        | Assign (a, f) ->
            let ca = self#compileAssignable compileContext a in
            let cav = self#compileAssignVar compileContext a in
            let cf = self#compileExpr compileContext f in
            fun context ->
                let value = match cf context with
                | `Array arr -> `Array (arr#copy ())
                | value -> value
                in
                begin try
                    let var = ca context in
                    var#set value
                with
                | _ ->
                    let var = object
                        val mutable v = value
                        method get = v
                        method set nv = v <- nv
                        end
                    in cav context var
                end;
                value
        | AssignByRef (a, e) ->
            let cav = self#compileAssignVar compileContext a in
            let ce = self#compileExprVar compileContext e in
            fun context ->
                let var = ce context in
                cav context var;
                var#get
        | ListAssign (l, e) ->
            let cl = self#compileListAssignElementList compileContext [] 0 l in
            let ce = self#compileExpr compileContext e in
            let rec getOffset e offsets =
                match offsets with
                | [] -> e
                | i::t -> match e with
                    | `Array a -> getOffset (a#get (string_of_int i))#get t
                    | _ -> failwith "list() assignee must be an array"
            in
            let doAssign context e (offsets, ca) =
                (ca context)#set (getOffset e offsets)
            in
            fun context ->
                let e = ce context in
                List.iter (doAssign context e) cl;
                e
        | BinaryAssign (op, a, f) -> self#compileExpr compileContext (Assign (a, BinaryOperation (op, Assignable a, f)))
        | PreInc a -> self#compileExpr compileContext (Assign (a, BinaryOperation (Plus, Assignable a, ConstValue (`Long 1))))
        | PostInc a ->
            let ret = self#compileExpr compileContext (Assignable a) in
            let inc = self#compileExpr compileContext (Assign (a, BinaryOperation (Plus, Assignable a, ConstValue (`Long 1)))) in
            fun context -> let _ = inc context in ret context
        | PreDec a -> self#compileExpr compileContext (Assign (a, BinaryOperation (Minus, Assignable a, ConstValue (`Long 1))))
        | PostDec a ->
            let ret = self#compileExpr compileContext (Assignable a) in
            let inc = self#compileExpr compileContext (Assign (a, BinaryOperation (Minus, Assignable a, ConstValue (`Long 1)))) in
            fun context -> let _ = inc context in ret context
        | Isset a ->
            let ca = self#compileAssignable compileContext a in
            fun context ->
                `Bool (
                    try
                        (ca context)#get <> `Null
                    with
                    | _ -> false
                )
        | Empty a ->
            let ca = self#compileAssignable compileContext a in
            fun context ->
                `Bool (
                    try
                        let `Bool b = to_bool (ca context)#get in not b
                    with
                    | _ -> false
                )
        | Include (filename, required, once) ->
            let cf = self#compileExpr compileContext filename in
            fun context -> let `String f = to_string (cf context) in context#files#includeFile f required once (fun l -> (self#compileFile (compileContext#setFile f) l) context)
        | Print e ->
            let ce = self#compileExpr compileContext e in
            fun context -> let `String s = to_string (ce context) in print_string s; `Long 1
    method private compileExprVar compileContext e =
        match e with
        | Assignable a ->
            let ca = self#compileAssignable compileContext a in
                fun context -> ca context
        | FunctionCall (name, argValues) ->
            let compiledArgs = List.map (self#compileExprVar compileContext) argValues in
            fun context ->
                let var = context#functions#exec
                    (context#resolveNamespace ~fallbackTest:(context#functions#has) name)
                    (List.map (fun cf -> cf context) compiledArgs)
                in var
        | Invoke (e, argValues) ->
            let ce = self#compileExpr compileContext e in
            let compiledArgs = List.map (self#compileExprVar compileContext) argValues in
            fun context -> invoke_callable (ce context) context (List.map (fun cf -> cf context) compiledArgs)
        | MethodCall (obj, methodName, argValues) ->
            let cobj = self#compileExpr compileContext obj in
            let compiledArgs = List.map (self#compileExprVar compileContext) argValues in
            fun context -> begin match cobj context with
                | `Object o -> (o#getObjectMethod context#callingClass methodName (List.map (fun cf -> cf context) compiledArgs))
                | _ -> raise BadType
            end
        | VariableMethodCall (obj, e, argValues) ->
            let cobj = self#compileExpr compileContext obj in
            let ce = self#compileExpr compileContext e in
            let compiledArgs = List.map (self#compileExprVar compileContext) argValues in
            fun context ->
                let `String methodName = to_string (ce context) in
                begin match cobj context with
                | `Object o -> (o#getObjectMethod context#callingClass methodName (List.map (fun cf -> cf context) compiledArgs))
                | _ -> raise BadType
            end
        | StaticMethodCall (classRef, methodName, argValues) -> begin
            let compiledArgs = List.map (self#compileExprVar compileContext) argValues in
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
                in (m (List.map (fun cf ->cf context) compiledArgs))
            end
        | VariableStaticMethodCall (classRef, e, argValues) -> begin
            let compiledArgs = List.map (self#compileExprVar compileContext) argValues in
            let ce = self#compileExpr compileContext e in
            fun context ->
                let `String methodName = to_string (ce context) in
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
                in (m (List.map (fun cf ->cf context) compiledArgs))
            end
        | _ ->
            let ce = self#compileExpr compileContext e in
            fun context -> new variable (ce context)
    method private compileAssignable compileContext a =
        match a with
        | Variable s -> fun context -> context#vars#find s
        | Superglobal s -> fun context -> context#vars#findSuperglobal s
        | VariableVariable e ->
            let ce = self#compileExpr compileContext e in
            fun context -> let `String s = to_string (ce context) in context#vars#find s
        | ArrayOffset (e, o) ->
            let ce = self#compileExpr compileContext e in
            let co = match o with None -> None | Some o -> Some (self#compileExpr compileContext o) in
            fun context ->
                begin match co with
                | None -> raise Expression.MissingArrayOffset
                | Some co -> match ce context with
                    | `String s -> let `Long i = to_long (co context) in new variable (`String (String.sub s i 1))
                    | `Array a -> let `String offset = to_string (co context) in a#get offset
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
            let cobj = self#compileExpr compileContext obj in
            fun context ->
                begin match cobj context with
                | `Object o -> o#getObjectPropertyVar context#callingClass propName
                | _ -> raise BadType
                end
    method private compileAssignVar compileContext a =
        match a with
        | Variable s -> fun context var -> context#vars#replace s var
        | Superglobal s -> fun context var -> context#vars#replaceSuperglobal s var
        | VariableVariable e ->
            let ce = self#compileExpr compileContext e in
            fun context var -> let `String s = to_string (ce context) in context#vars#replace s var
        | ArrayOffset (e, o) ->
            let ce = self#compileExpr compileContext e in
            let co = match o with None -> None | Some o -> Some (self#compileExpr compileContext o) in
            fun context var ->
                let arr = match ce context with `Array arr -> arr | _ -> raise BadType in
                begin match co with
                | None -> arr#set (arr#nextOffset) var
                | Some co -> let `String offset = to_string (co context) in arr#set offset var
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
            let cobj = self#compileExpr compileContext obj in
            fun context var ->
                begin match cobj context with
                | `Object o -> o#setObjectPropertyVar context#callingClass propName var
                | _ -> raise BadType
                end
    method compileArgConf compileContext (name, isRef, typeHint, defaultValue) =
        let compiledDefault = match defaultValue with
            | None -> None
            | Some e -> Some (self#compileExpr compileContext e)
        in
        (name, isRef, typeHint, compiledDefault)
    method compileListAssignElementList compileContext offsets index l =
        match l with
        | [] -> []
        | lae::t -> (self#compileListAssignElementList compileContext offsets (index+1) t) @ (self#compileListAssignElement compileContext (offsets@[index]) lae)
    method compileListAssignElement compileContext offsets lae =
        match lae with
        | LAE_None -> []
        | LAE_Assignable a -> [(offsets, self#compileAssignable compileContext a)]
        | LAE_List l -> self#compileListAssignElementList compileContext offsets 0 l
end

