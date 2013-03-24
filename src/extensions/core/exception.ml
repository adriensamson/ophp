open Interpreter

let exceptionConstruct self obj args =
    let num = List.length args in
    if num > 0 then
        obj#setObjectProperty (Some self) "message" (List.nth args 0)#get;
    if num > 1 then
        obj#setObjectProperty (Some self) "code" (List.nth args 1)#get;
    if num > 2 then
        obj#setObjectProperty (Some self) "previous" (List.nth args 2)#get;
    object method get = `Null method set = fun _ -> () end
    

let (exceptionClass : (('v PhpArray.phpArray, 'v Object.phpObject)  Language.Typing.value as 'v) Object.phpClass)
    = new Object.phpClass "Exception" false false false false None []
    []
    [
        ("message", false, Language.Typing.Protected, `Null);
        ("code", false, Language.Typing.Protected, `Null);
        ("previous", false, Language.Typing.Private, `Null)
    ]
    [
        ("__construct", Language.Typing.Public, exceptionConstruct);
        ("getMessage", Language.Typing.Public, fun self obj args -> obj#getObjectPropertyVar (Some self) "message")
    ]
    []
    []

let _ = Extension.register
    "core/exception"
    []
    []
    [("Exception", exceptionClass)]
    []

