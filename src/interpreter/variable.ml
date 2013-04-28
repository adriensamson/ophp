class ['v] variable =
    object (self)
    val mutable v = None
    method get = match v with
        | None -> raise Not_found
        | Some value -> value
    method set (nv : 'v) = v <- Some nv
    end

class ['v] variableRegistry =
    object (self)
        val globalScope = None
        val parentScope = None
        val staticVariables = Hashtbl.create 10
        val variables = Hashtbl.create 10
        val functionName = ""
        method find (name : string) =
            try
                Hashtbl.find variables name
            with
            | Not_found ->
                let v = new variable in
                Hashtbl.replace variables name v;
                v
        method replace name (var : 'v variable) = Hashtbl.replace variables name var
        method newScope functionName =
            {< globalScope = if globalScope = None then Some self else globalScope;
            parentScope = Some self;
            variables = Hashtbl.create 10;
            functionName = functionName >}
        method addFromParent ?(byRef=false) name =
            match parentScope with
            | None -> failwith "No parent scope"
            | Some p ->
                if byRef then
                    self#replace name (p#find name)
                else
                    (self#find name)#set (p#find name)#get
        method addFromGlobal name =
            match globalScope with
            | None -> failwith "No global scope"
            | Some g -> self#replace name (g#find name)
        method addFromStatic name defaultValue =
            let var =
                try
                    Hashtbl.find staticVariables (functionName, name)
                with
                | Not_found ->
                    let v = new variable in
                    v#set defaultValue;
                    Hashtbl.replace staticVariables (functionName, name) v;
                    v
            in self#replace name var
    end

