open Language.Typing
open Language.Ast

class type obj = object end

let checkTypeHint localContext typeHint value = match typeHint, value with
    | NoTypeHint, _
    | ArrayTypeHint, `Array _ -> ()
    | ClassTypeHint name, `Object o when o#instanceOf (localContext#classes#get (localContext#resolveNamespace ?fallbackTest:None name)) -> ()
    | _, _ -> failwith "Bad Type hint"

let assignParam localContext name byRef var =
    let var = if byRef then var else begin
        let v = match var#get with
            | `Array a -> `Array (a#copy ())
            | a -> a
        in
        new variable v
    end in
    localContext#vars#replace name var

let checkParam localContext typeHint var =
    checkTypeHint localContext typeHint var#get

let rec assignParams localContext argConfs vars =
    match argConfs, vars with
    | (n, r, _, _)::acs, v::vs -> assignParam localContext n r v; assignParams localContext acs vs
    | [], _ -> ()
    | (n, r, _, Some d)::acs, [] -> assignParam localContext n r (new variable (d localContext)); assignParams localContext acs []
    | (_, _, _, None)::acs, [] -> failwith "Missing arguments"

let rec checkParams localContext argConfs vars =
    match argConfs, vars with
    | (_, _, t, _)::acs, v::vs -> (checkParam localContext t v; v) :: (checkParams localContext acs vs)
    | [], v -> v
    | (_, _, _, Some d)::acs, [] -> (new variable (d localContext)) :: (checkParams localContext acs [])
    | (_, _, _, None)::acs, [] -> failwith "Missing arguments"

class ['a, 'o, 'c, 'e] baseFunction argConf returnByRef (code : 'e -> (('a, 'o) value variable as 'v) list -> 'v) =
object (self)
    method private makeContext (context : 'e) _ =
        context#functionScope ?callingClass:None ?obj:None ?staticClass:None (self :> obj)

    method exec context args =
        let args = checkParams context argConf args in
        code (self#makeContext context args) args
end

class ['a, 'o, 'c, 'e] phpFunction argConf returnByRef code =
object (self)
    inherit ['a, 'o, 'c, 'e] baseFunction argConf returnByRef (fun context _ -> code context) as parent
    method private makeContext context args =
        let localContext = parent#makeContext context args in
        assignParams localContext argConf args;
        localContext
end

class ['a, 'o, 'c, 'e] phpClosure argConf returnByRef code uses =
object (self)
    inherit ['a, 'o, 'c, 'e] phpFunction argConf returnByRef code as parent
    method private makeContext context args =
        let localContext = parent#makeContext context args in
        List.iter (fun (name, byRef) -> localContext#vars#addFromParent ?byRef:(Some byRef) name) uses;
        localContext
end

class ['a, 'o, 'c, 'e] phpStaticMethod argConf returnByRef (code : 'e -> ('a, 'o) value variable) (context : 'e) (callingClass : 'c) =
object (self)
    method private makeContext context staticClass args =
        let localContext = context#functionScope ?callingClass:(Some callingClass) ?obj:None ?staticClass:(Some staticClass) (self :> obj) in
        assignParams localContext argConf args;
        localContext

    method exec staticClass args =
        let args = checkParams context argConf args in
        code (self#makeContext context staticClass args)
end

class ['a, 'o, 'c, 'e] phpMethod argConf returnByRef (code : 'e -> ('a, 'o) value variable) (context : 'e) (callingClass : 'c) =
object (self)
    method private makeContext context obj args =
        let localContext = context#functionScope ?callingClass:(Some callingClass) ?obj:(Some obj) ?staticClass:None (self :> obj) in
        assignParams localContext argConf args;
        localContext

    method exec obj args =
        let args = checkParams context argConf args in
        code (self#makeContext context obj args)
end

