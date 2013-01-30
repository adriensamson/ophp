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
    (methodsL : (string * Language.Typing.visibility * (phpClass -> phpObject -> Language.Typing.value list -> Language.Typing.value)) list)
    (staticMethodsL : (string * Language.Typing.visibility * (phpClass -> phpClass -> Language.Typing.value list -> Language.Typing.value)) list)
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
        
        method getConstant constantName =
            let rec find l = match l with
                | [] -> raise Not_found
                | (name, value)::_ when name = constantName -> value
                | _::t -> find t
            in find constants
        
        method getStaticProperty callingClass propName =
            let (vis, value) = Hashtbl.find staticProperties propName in
            value
        
        method setStaticProperty callingClass propName value =
            let (vis, _) = Hashtbl.find staticProperties propName in
            Hashtbl.replace staticProperties propName (vis, value)
        
        method getStaticMethod finalClass callingClass methodName =
            let (vis, f) = Hashtbl.find staticMethods methodName in
            f (self :> phpClass) finalClass

        method getMethod obj callingClass methodName =
            let (vis, f) = Hashtbl.find methods methodName in
            f (self :> phpClass) obj

        method newObject l =
            let o = new phpObject (self :> phpClass) (List.map (fun (name, _, vis, value) -> (name, value)) (List.filter (fun (_, isStatic, _, _) -> not isStatic) propertiesL)) in
            o
        
        initializer
            List.iter (fun (name, _, vis, value) -> Hashtbl.replace staticProperties name (vis, value)) (List.filter (fun (_, isStatic, _, _) -> isStatic) propertiesL);
            List.iter (fun (name, vis, f) -> Hashtbl.replace staticMethods name (vis, f)) staticMethodsL;
            List.iter (fun (name, vis, f) -> Hashtbl.replace methods name (vis, f)) methodsL
            
    end

and phpObject
    objectClass
    properties
    = object (self : Language.Typing.value #Language.Typing.phpObject)
        val mutable properties = properties;
        
        method objectClass = objectClass
        
        method getProperty callingClass propName =
            let rec find l = match l with
                | [] -> raise Not_found
                | (name, value)::_ when name = propName -> value
                | _::t -> find t
            in find properties
            
        method setProperty callingClass propName value =
            let rec replace l = match l with
                | [] -> raise Not_found
                | (name, _)::t when name = propName -> (name, value)::t
                | a::t -> a::(replace t)
            in properties <- replace properties
        
        method getMethod = objectClass#getMethod (self :> phpObject)
    end

