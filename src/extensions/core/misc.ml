let _ = Interpreter.Extension.register
    "core/misc"
    []
    [("define", fun c l -> let `String name = Language.Typing.to_string (List.nth l 0)#get in c#constants#add name (List.nth l 1)#get; object method get = `Bool true method set = failwith "read_only" end)]
    []
    []

