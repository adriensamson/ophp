open Language.Typing

let tryParent o = match o with
    | None -> raise Not_found
    | Some a -> a

let getSome = tryParent

let findWithParents f phpClass callingClass name =
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

class ['v] phpClass
    (name : string)
    (isStatic : bool)
    (isAbstract : bool)
    (isFinal : bool)
    (isInterface : bool)
    (parent : 'v phpClass option)
    (implements : 'v phpClass list)
    (abstractMethods : (string * bool * bool * Language.Typing.visibility * (string * bool * Language.Ast.typeHint) list) list)
    = object (self)
    
        val staticProperties = ((new Bag.bag 10) : (string, visibility * 'v variable) Bag.bag)
        val staticMethods = ((new Bag.bag 10) : (string, visibility * < exec : 'v phpClass -> 'v variable list -> 'v variable >) Bag.bag)
        val methods = ((new Bag.bag 10) : (string, visibility * < exec : 'v phpObject -> 'v variable list -> 'v variable >) Bag.bag)
        val constants = ((new Bag.bag 10) : (string, 'v) Bag.bag)
        val properties = ((new Bag.bag 10) : (string, visibility * 'v) Bag.bag)

        method staticProperties = staticProperties
        method staticMethods = staticMethods
        method methods = methods
        method constants = constants
        method properties = properties
        
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
            constants#get constantName
        
        method findStaticProperty propName =
            staticProperties#get propName
        
        method replaceStaticProperty propName =
            let (vis, _) = staticProperties#get propName in
            (vis, fun value -> staticProperties#set propName (vis, value))
        
        method getStaticMethod methodName =
            staticMethods#get methodName
        
        method getMethod methodName =
            methods#get methodName
        
        method newObject l cc =
            let o = new phpObject (self :> 'v phpClass) in
            self#initObject o;
            begin try
                let (vis, f) = self#getMethod "__construct" in
                if vis = Public || vis = Protected && (match cc with Some c -> c#instanceOf (self :> 'v phpClass) | None -> false) || cc = Some (self :> 'v phpClass) then
                    let _ = f#exec o l in ()
                else raise BadVisibility
            with
            | Not_found -> ()
            end;
            o
        method initObject o =
            begin
                match parent with
                    | None -> ()
                    | Some c -> c#initObject o
            end;
            o#addProperties
                (self :> 'v phpClass)
                (List.map (fun (name, (vis, value)) -> (name, vis, value)) properties#all)
        
        method getClassConstant name =
            try
                self#getConstant name
            with
                | Not_found -> (tryParent parent)#getClassConstant name
        method getClassStaticProperty cc n =
            (findWithParents (fun c -> c#findStaticProperty) (self :> 'v phpClass) cc n)#get
        method setClassStaticProperty cc n =
            (findWithParents (fun c -> c#findStaticProperty) (self :> 'v phpClass) cc n)#set
        method getClassStaticPropertyVar cc n =
            (findWithParents (fun c -> c#findStaticProperty) (self :> 'v phpClass) cc n)
        method setClassStaticPropertyVar cc n =
            (findWithParents (fun c -> c#replaceStaticProperty) (self :> 'v phpClass) cc n)
        method getClassStaticMethod =
            (findWithParents (fun c -> c#getStaticMethod)) (self :> 'v phpClass)
        method getClassMethod =
            (findWithParents (fun c -> c#getMethod)) (self :> 'v phpClass)
    end

and ['v] phpObject
    objectClass
    = object (self)
        val properties = new Bag.bag 10;
        
        method objectClass = objectClass
        method instanceOf = objectClass#instanceOf
        
        method findProperty prop =
            properties#get prop
            
        method replaceProperty prop =
            let (vis, _) = properties#get prop in
            (vis, fun value -> properties#set prop (vis, value))
        
        method addProperties phpClass propertiesL =
            List.iter (fun (name, vis, value) -> properties#set (phpClass, name) (vis, new variable value)) propertiesL
        
        method getObjectProperty cc n =
            (findWithParents (fun c name -> self#findProperty (c, name)) self#objectClass cc n)#get
        method setObjectProperty cc n =
            (findWithParents (fun c name -> self#findProperty (c, name)) self#objectClass cc n)#set
        method getObjectPropertyVar cc n =
            (findWithParents (fun c name -> self#findProperty (c, name)) self#objectClass cc n)
        method setObjectPropertyVar cc n =
            (findWithParents (fun c name -> self#replaceProperty (c, name)) self#objectClass cc n)
        method getObjectMethod callingClass methodName =
            (self#objectClass#getClassMethod callingClass methodName)#exec (self :> 'v phpObject)
    end

