class functionRegistry =
    object
        val functions = Hashtbl.create 10
        method add (name : string) (f : Language.Typing.value list -> Language.Typing.value) =
            Hashtbl.add functions name f
        method exec name argValues =
            let f = Hashtbl.find functions name in
            f argValues
    end

let registry = new functionRegistry

