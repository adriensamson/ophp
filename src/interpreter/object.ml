open Language.Typing

let tryParent o = match o with
    | None -> raise Not_found
    | Some a -> a

let getSome = tryParent

class phpClass
    (name : string)
    (isStatic : bool)
    (isAbstract : bool)
    (isFinal : bool)
    (isInterface : bool)
    (parent : phpClass option)
    (implements : phpClass list)
    (constants : (string * Language.Typing.value) list)
    (propertiesL : (string * bool * Language.Typing.visibility * Language.Typing.value) list)
    (methodsL : (string * Language.Typing.visibility * (Language.Typing.value Language.Typing.phpClass -> Language.Typing.value Language.Typing.phpObject -> Language.Typing.value list -> Language.Typing.value)) list)
    (staticMethodsL : (string * Language.Typing.visibility * (Language.Typing.value Language.Typing.phpClass -> Language.Typing.value Language.Typing.phpClass -> Language.Typing.value list -> Language.Typing.value)) list)
    (abstractMethods : (string * bool * Language.Typing.visibility * string list) list)
    = object (self : Language.Typing.value #Language.Typing.phpClass)
    
        val staticProperties = Hashtbl.create 10
        val staticMethods = Hashtbl.create 10
        val methods = Hashtbl.create 10
        method name = name
        method abstract = isAbstract
        method static = isStatic
        method final = isFinal
        method interface = isInterface
        method parent = parent
        method implements = implements
        
        method instanceOf c = if c = (self :> phpClass) then true else match parent with
            | None -> false
            | Some p -> p#instanceOf c
        
        method getConstant constantName =
            let rec find l = match l with
                | [] -> raise Not_found
                | (name, value)::_ when name = constantName -> value
                | _::t -> find t
            in find constants
        
        method getStaticProperty propName =
            Hashtbl.find staticProperties propName
        
        method setStaticProperty propName =
            let (vis, _) = Hashtbl.find staticProperties propName in
            (vis, fun value -> Hashtbl.replace staticProperties propName (vis, value))
        
        method getStaticMethod methodName =
            Hashtbl.find staticMethods methodName
        
        method getMethod methodName =
            Hashtbl.find methods methodName
        
        method newObject l =
            let o = new phpObject (self :> phpClass) (List.map (fun (name, _, vis, value) -> (name, vis, value)) (List.filter (fun (_, isStatic, _, _) -> not isStatic) propertiesL)) in
            o
        
        initializer
            List.iter (fun (name, _, vis, value) -> Hashtbl.replace staticProperties name (vis, value)) (List.filter (fun (_, isStatic, _, _) -> isStatic) propertiesL);
            List.iter (fun (name, vis, f) -> Hashtbl.replace staticMethods name (vis, f (self :> phpClass))) staticMethodsL;
            List.iter (fun (name, vis, f) -> Hashtbl.replace methods name (vis, f (self :> phpClass))) methodsL
            
    end

and phpObject
    objectClass
    propertiesL
    = object (self : Language.Typing.value #Language.Typing.phpObject)
        val properties = Hashtbl.create 10;
        
        method objectClass = objectClass
        
        method getProperty prop =
            Hashtbl.find properties prop
            
        method setProperty prop =
            let (vis, _) = Hashtbl.find properties prop in
            (vis, fun value -> Hashtbl.replace properties prop (vis, value))
        
        initializer
            List.iter (fun (name, vis, value) -> Hashtbl.replace properties (objectClass, name) (vis, value)) propertiesL
    end


let findWithParents f (phpClass : phpClass) callingClass name =
    let isInstance = try
        phpClass#instanceOf (getSome callingClass)
    with
        | Not_found -> false
    in
    try
        if isInstance then
            let (vis, value) = f (getSome callingClass) name in
            if vis = Private then value else raise Not_found
        else raise Not_found
    with Not_found ->
        let rec f' c =
            try
                let (vis, value) = f c name in
                match (vis, isInstance) with
                    | Public, _
                    | Protected, true -> value
                    | _ -> raise Not_found
            with
                | Not_found -> f' (tryParent c#parent)
        in f' phpClass

let getClassStaticProperty =
    findWithParents (fun c -> c#getStaticProperty)

let setClassStaticProperty =
    findWithParents (fun c -> c#setStaticProperty)

let getClassStaticMethod =
    findWithParents (fun c -> c#getStaticMethod)

let rec getClassMethod =
    findWithParents (fun c -> c#getMethod)

let rec getObjectProperty obj callingClass propName =
    let (vis, value) = obj#getProperty (obj#objectClass, propName) in
    value

let rec setObjectProperty obj callingClass propName =
    let (vis, f) = obj#setProperty (obj#objectClass, propName) in
    f
let getObjectMethod obj callingClass methodName =
    getClassMethod obj#objectClass callingClass methodName obj

