class variableRegistry =
    object
        val variables = Hashtbl.create 10
        method set (name : string) (value : Language.Typing.value) =
            Hashtbl.replace variables name value
        method get name =
            Hashtbl.find variables name
    end

