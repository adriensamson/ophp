open Language.Typing

let tryParent o = match o with
    | None -> raise Not_found
    | Some a -> a

let getSome = tryParent

class ['v] variable (value : 'v) =
    object
    val mutable v = value
    method get = v
    method set nv = v <- nv
    end

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
        
        method findStaticProperty propName =
            Hashtbl.find staticProperties propName
        
        method replaceStaticProperty propName =
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
            List.iter (fun (name, _, vis, value) -> Hashtbl.replace staticProperties name (vis, new variable value)) (List.filter (fun (_, isStatic, _, _) -> isStatic) propertiesL);
            List.iter (fun (name, vis, f) -> Hashtbl.replace staticMethods name (vis, f (self :> 'v phpClass))) staticMethodsL;
            List.iter (fun (name, vis, f) -> Hashtbl.replace methods name (vis, f (self :> 'v phpClass))) methodsL
            
    end

and ['v] phpObject
    objectClass
    = object (self)
        val properties = Hashtbl.create 10;
        
        method objectClass = objectClass
        
        method findProperty prop =
            Hashtbl.find properties prop
            
        method replaceProperty prop =
            let (vis, _) = Hashtbl.find properties prop in
            (vis, fun value -> Hashtbl.replace properties prop (vis, value))
        
        method addProperties phpClass propertiesL =
            List.iter (fun (name, vis, value) -> Hashtbl.replace properties (phpClass, name) (vis, new variable value)) propertiesL
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

let getClassStaticProperty c cc n =
    (findWithParents (fun c -> c#findStaticProperty) c cc n)#get

let setClassStaticProperty c cc n =
    (findWithParents (fun c -> c#findStaticProperty) c cc n)#set

let getClassStaticPropertyVar c cc n =
    (findWithParents (fun c -> c#findStaticProperty) c cc n)

let setClassStaticPropertyVar c cc n =
    (findWithParents (fun c -> c#replaceStaticProperty) c cc n)

let getClassStaticMethod c =
    findWithParents (fun c -> c#getStaticMethod) c

let getClassMethod c =
    findWithParents (fun c -> c#getMethod) c

let getObjectProperty obj cc n =
    (findWithParents (fun c name -> obj#findProperty (c, name)) obj#objectClass cc n)#get

let setObjectProperty obj cc n =
    (findWithParents (fun c name -> obj#findProperty (c, name)) obj#objectClass cc n)#set

let getObjectPropertyVar obj cc n =
    (findWithParents (fun c name -> obj#findProperty (c, name)) obj#objectClass cc n)

let setObjectPropertyVar obj cc n =
    (findWithParents (fun c name -> obj#replaceProperty (c, name)) obj#objectClass cc n)

let getObjectMethod obj callingClass methodName =
    getClassMethod obj#objectClass callingClass methodName obj

