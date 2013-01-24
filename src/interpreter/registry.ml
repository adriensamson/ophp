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


let functions = new functionRegistry

let classes = new classRegistry

