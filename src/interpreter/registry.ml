class functionRegistry =
    object
        val functions = Hashtbl.create 10
        method add (name : string) (f : Language.Typing.value list -> Language.Typing.value) =
            Hashtbl.add functions name f
        method exec name argValues =
            let f = Hashtbl.find functions name in
            f argValues
    end

class classRegistry =
    object
        val classes = Hashtbl.create 10
        method add (name : string) (c : Language.Typing.value Language.Typing.phpClass) =
            Hashtbl.add classes name c
        method get name = Hashtbl.find classes name
    end



class fileRegistry
    parse
    = object (self)
        val filesOnce = Hashtbl.create 10
        method includeFile filename required once (exec : Language.Ast.stmt list -> Language.Typing.value) =
            if not once || not (Hashtbl.mem filesOnce filename) then
                try
                    let result = exec (self#parseChannel (open_in filename)) in
                    if once then Hashtbl.replace filesOnce filename true;
                    result
                with
                    | Sys_error _ as e -> if not required then `Bool false else raise e
            else `Bool true
        
        method parseChannel chan =
            parse chan
    end

