class variable =
    object (self)
    val mutable v = None
    method get = match v with
        | None -> raise Not_found
        | Some value -> value
    method set (nv : Language.Typing.value) = v <- Some nv
    end

class variableRegistry =
    object (self)
        val globalScope = None
        val parentScope = None
        val variables = Hashtbl.create 10
        method find (name : string) =
            try
                Hashtbl.find variables name
            with
            | Not_found ->
                let v = new variable in
                Hashtbl.replace variables name v;
                v
        method replace name (var : variable) = Hashtbl.replace variables name var
        method newScope () =
            {< globalScope = if globalScope = None then Some self else globalScope;
            parentScope = Some self;
            variables = Hashtbl.create 10 >}
        method addFromParent name =
            match parentScope with
            | None -> failwith "No parent scope"
            | Some p -> self#replace name (p#find name)
        method addFromGlobal name =
            match globalScope with
            | None -> failwith "No global scope"
            | Some g -> self#replace name (g#find name)
    end

