class ['v] constantRegistry =
    object
        val constants = Hashtbl.create 10
        method add (name : string) (value: 'v) =
            Hashtbl.add constants name value
        method get name =
            Hashtbl.find constants name
        method has name = Hashtbl.mem constants name
    end

class ['var] functionRegistry =
    object
        val functions = Hashtbl.create 10
        method add (name : string) (f : 'var list -> 'var) =
            Hashtbl.add functions name f
        method exec name argValues =
            let f = Hashtbl.find functions name in
            f argValues
        method has name = Hashtbl.mem functions name
    end

class ['c] classRegistry =
    object
        val classes = Hashtbl.create 10
        method add (name : string) (c : 'c) =
            Hashtbl.add classes name c
        method get name = Hashtbl.find classes name
    end



class ['v] fileRegistry
    parse
    = object (self)
        val filesOnce = Hashtbl.create 10
        method includeFile filename required once (exec : Language.Ast.namespaceStmt list -> 'v) =
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

