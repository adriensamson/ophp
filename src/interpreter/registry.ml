class ['v] constantRegistry =
    object
        inherit [string, 'v] Bag.bag 10 as parent
        method get name =
            try
                parent#get name
            with
            | Not_found -> failwith (Printf.sprintf "Constant %s not found" name)
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
        inherit [string, 'c] Bag.bag 10 as parent
        val mutable autoload = fun _ -> ()
        method setAutoload a = autoload <- a
        method get name =
            try
                begin if not (parent#has name) then
                    autoload name
                end;
                parent#get name
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
        method resolveFilename f =
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

