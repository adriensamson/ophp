class classRegistry =
    object
        val classes = Hashtbl.create 10
        method add (name : string) (c : Language.Typing.value phpClass) =
            Hashtbl.add classes name c
        method get name = Hashtbl.find classes name
    end

let registry = new classRegistry
