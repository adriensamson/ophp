open Interpreter

let exceptionConstruct self = object method exec obj args =
    let num = List.length args in
    if num > 0 then
        obj#setObjectProperty (Some self) "message" (List.nth args 0)#get;
    if num > 1 then
        obj#setObjectProperty (Some self) "code" (List.nth args 1)#get;
    if num > 2 then
        obj#setObjectProperty (Some self) "previous" (List.nth args 2)#get;
    object method get = `Null method set = fun _ -> () end
end

let (exceptionClass : (('v PhpArray.phpArray, 'v Object.phpObject)  Language.Typing.value as 'v) Object.phpClass)
    = new Object.phpClass "Exception" false false false false None [] []
    
let _ =
    exceptionClass#properties#set "message" (Language.Typing.Protected, `Null);
    exceptionClass#properties#set "code" (Language.Typing.Protected, `Null);
    exceptionClass#properties#set "previous" (Language.Typing.Private, `Null);
    exceptionClass#methods#set "__construct" (Language.Typing.Public, exceptionConstruct exceptionClass);
    exceptionClass#methods#set "getMessage" (Language.Typing.Public, object method exec obj args = obj#getObjectPropertyVar (Some exceptionClass) "message" end)

let _ = Extension.register
    "core/exception"
    []
    []
    [("Exception", exceptionClass)]
    []

