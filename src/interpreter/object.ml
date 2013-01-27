class phpClass
    (name : string)
    (isStatic : bool)
    (isAbstract : bool)
    (isFinal : bool)
    (isInterface : bool)
    (parent : phpClass option)
    (implements : phpClass list)
    (constants : (string * Language.Typing.value) list)
    (properties : (string * bool * Language.Typing.visibility * Language.Typing.value) list)
    (methods : (string * Language.Typing.visibility * (phpClass -> phpObject -> Language.Typing.value list -> Language.Typing.value)) list)
    (staticMethods : (string * Language.Typing.visibility * (phpClass -> Language.Typing.value list -> Language.Typing.value)) list)
    (abstractMethods : (string * bool * Language.Typing.visibility * string list) list)
    = object (self : Language.Typing.value #Language.Typing.phpClass)
    
        val mutable staticProperties = List.map (fun (name, _, vis, value) -> (name, vis, value)) (List.filter (fun (_, isStatic, _, _) -> isStatic) properties)
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
            let rec find l = match l with
                | [] -> raise Not_found
                | (name, _, value)::_ when name = propName -> value
                | _::t -> find t
            in find staticProperties
        
        method setStaticProperty callingClass propName value =
            let rec replace l = match l with
                | [] -> raise Not_found
                | (name, vis, _)::t when name = propName -> (name, vis, value)::t
                | a::t -> a::(replace t)
            in staticProperties <- replace staticProperties
        
        method getStaticMethod callingClass methodName =
            let rec find l = match l with
                | [] -> raise Not_found
                | (name, _, f)::_ when name = methodName -> f
                | _::t -> find t
            in find staticMethods (self :> phpClass)

        method getMethod obj callingClass methodName =
            let rec find l = match l with
                | [] -> raise Not_found
                | (name, _, f)::_ when name = methodName -> f
                | _::t -> find t
            in find methods (self :> phpClass) obj

        method newObject l =
            let o = new phpObject (self :> phpClass) (List.map (fun (name, _, vis, value) -> (name, value)) (List.filter (fun (_, isStatic, _, _) -> not isStatic) properties)) in
            o
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

