class variableRegistry =
    object (self)
        val globalScope = None
        val parentScope = None
        val variables = Hashtbl.create 10
        method set (name : string) (value : Language.Typing.value) =
            Hashtbl.replace variables name value
        method get name =
            Hashtbl.find variables name
        method newScope () =
            {< globalScope = if globalScope = None then Some self else globalScope;
            parentScope = Some self;
            variables = Hashtbl.create 10 >}
        method addFromParent name =
            match parentScope with
            | None -> failwith "No parent scope"
            | Some p -> self#set name (p#get name)
        method addFromGlobal name =
            match globalScope with
            | None -> failwith "No global scope"
            | Some g -> self#set name (g#get name)
    end

