class ['v] constantRegistry =
    object
        val constants = Hashtbl.create 10
        method add (name : string) (value: 'v) =
            Hashtbl.add constants name value
        method get name =
            try
                Hashtbl.find constants name
            with
            | Not_found -> failwith (Printf.sprintf "Constant %s not found" name)
        method has name = Hashtbl.mem constants name
    end

class ['var] functionRegistry =
    object
        val functions = Hashtbl.create 10
        method add (name : string) (f : 'var list -> 'var) =
            Hashtbl.add functions name f
        method exec name argValues =
            try
                let f = Hashtbl.find functions name in
                f argValues
            with
            | Not_found -> failwith (Printf.sprintf "Function %s not found" name)
        method has name = Hashtbl.mem functions name
    end

class ['c] classRegistry =
    object
        val classes = Hashtbl.create 10
        val mutable autoload = fun _ -> ()
        method setAutoload a = autoload <- a
        method add (name : string) (c : 'c) = Hashtbl.add classes name c
        method has name = Hashtbl.mem classes name
        method get name =
            try
                begin if not (Hashtbl.mem classes name) then
                    autoload name
                end;
                Hashtbl.find classes name
            with
            | Not_found -> failwith (Printf.sprintf "Class %s not found" name)
    end



class ['v] fileRegistry
    parse
    = object (self)
        val filesOnce = Hashtbl.create 10
        val mutable includePaths = ["."]
        method includePaths = includePaths
        method setIncludePaths p = includePaths <- p
        method private resolveFilename f =
            let rec resolve paths = match paths with
                | [] -> failwith (Printf.sprintf "File %s not found" f)
                | p::l ->
                    let filename = p ^ "/" ^ f in
                    try
                        Unix.access filename [Unix.F_OK; Unix.R_OK];
                        filename
                    with
                    | Unix.Unix_error _ -> resolve l
            in
            if f.[0] = '/' then f else resolve includePaths
        method includeFile filename required once (exec : Language.Ast.namespaceStmt list -> 'v) =
            let filename = self#resolveFilename filename in
            if not once || not (Hashtbl.mem filesOnce filename) then
                try
                    print_endline filename;
                    let result = exec (self#parseChannel (open_in filename)) in
                    if once then Hashtbl.replace filesOnce filename true;
                    result
                with
                    | Sys_error _ as e -> if not required then `Bool false else raise e
            else `Bool true
        
        method parseChannel chan =
            parse chan
    end

