open Language.Typing

let tryParent o = match o with
    | None -> raise Not_found
    | Some a -> a

let getSome = tryParent

class ['v] phpClass
    (name : string)
    (isStatic : bool)
    (isAbstract : bool)
    (isFinal : bool)
    (isInterface : bool)
    (parent : 'v phpClass option)
    (implements : 'v phpClass list)
    (constantsL : (string * 'v) list)
    (propertiesL : (string * bool * Language.Typing.visibility * 'v) list)
    (methodsL : (string * Language.Typing.visibility * ('v phpClass -> 'v phpObject -> 'v list -> 'v)) list)
    (staticMethodsL : (string * Language.Typing.visibility * ('v phpClass -> 'v phpClass -> 'v list -> 'v)) list)
    (abstractMethods : (string * bool * Language.Typing.visibility * string list) list)
    = object (self)
    
        val staticProperties = Hashtbl.create 10
        val staticMethods = Hashtbl.create 10
        val methods = Hashtbl.create 10
        val constants = Hashtbl.create 10
        method name = name
        method abstract = isAbstract
        method static = isStatic
        method final = isFinal
        method interface = isInterface
        method parent = parent
        method implements = implements
        
        method instanceOf c = if c = (self :> 'v phpClass) then true else match parent with
            | None -> false
            | Some p -> p#instanceOf c
        
        method getConstant constantName =
            Hashtbl.find constants constantName
        
        method getStaticProperty propName =
            Hashtbl.find staticProperties propName
        
        method setStaticProperty propName =
            let (vis, _) = Hashtbl.find staticProperties propName in
            (vis, fun value -> Hashtbl.replace staticProperties propName (vis, value))
        
        method getStaticMethod methodName =
            Hashtbl.find staticMethods methodName
        
        method getMethod methodName =
            Hashtbl.find methods methodName
        
        method newObject (l : 'v list) =
            let o = new phpObject (self :> 'v phpClass) in
            self#initObject o;
            o
        method initObject o =
            begin
                match parent with
                    | None -> ()
                    | Some c -> c#initObject o
            end;
            o#addProperties (self :> 'v phpClass) (List.map (fun (name, _, vis, value) -> (name, vis, value)) (List.filter (fun (_, isStatic, _, _) -> not isStatic) propertiesL))
        
        initializer
            List.iter (fun (name, value) -> Hashtbl.replace constants name value) constantsL;
            List.iter (fun (name, _, vis, value) -> Hashtbl.replace staticProperties name (vis, value)) (List.filter (fun (_, isStatic, _, _) -> isStatic) propertiesL);
            List.iter (fun (name, vis, f) -> Hashtbl.replace staticMethods name (vis, f (self :> 'v phpClass))) staticMethodsL;
            List.iter (fun (name, vis, f) -> Hashtbl.replace methods name (vis, f (self :> 'v phpClass))) methodsL
            
    end

and ['v] phpObject
    objectClass
    = object (self)
        val properties = Hashtbl.create 10;
        
        method objectClass = objectClass
        
        method getProperty prop =
            Hashtbl.find properties prop
            
        method setProperty prop =
            let (vis, _) = Hashtbl.find properties prop in
            (vis, fun value -> Hashtbl.replace properties prop (vis, value))
        
        method addProperties phpClass propertiesL =
            List.iter (fun (name, vis, value) -> Hashtbl.replace properties (phpClass, name) (vis, value)) propertiesL
    end

let rec getClassConstant phpClass name =
    try
        phpClass#getConstant name
    with
        | Not_found -> getClassConstant (tryParent phpClass#parent) name

let findWithParents f (phpClass : 'v phpClass) callingClass name =
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

let getClassStaticProperty c =
    findWithParents (fun c -> c#getStaticProperty) c

let setClassStaticProperty c =
    findWithParents (fun c -> c#setStaticProperty) c

let getClassStaticMethod c =
    findWithParents (fun c -> c#getStaticMethod) c

let getClassMethod c =
    findWithParents (fun c -> c#getMethod) c

let getObjectProperty obj =
    findWithParents (fun c name -> obj#getProperty (c, name)) obj#objectClass

let rec setObjectProperty obj =
    findWithParents (fun c name -> obj#setProperty (c, name)) obj#objectClass

let getObjectMethod obj callingClass methodName =
    getClassMethod obj#objectClass callingClass methodName obj

