open Interpreter

let (closureClass : Sig.phpClass)
    = new Object.phpClass "Closure" false false true false None [] []

let _ = closureClass#methods#set "__construct" (Language.Typing.Private, object method exec _ _ = failwith "Non instanciable" end)

class closureObject
    (f: Sig.variable list -> Sig.variable)
= object (self)
    inherit [Sig.value] Interpreter.Object.phpObject closureClass as parent
    method getObjectMethod callingClass methodName =
        if methodName = "__invoke" then
            f
        else
            parent#getObjectMethod callingClass methodName
end

let _ = Extension.register
    "core/closure"
    []
    []
    [("Closure", closureClass)]
    [fun context -> context#setClosureFactory (fun f -> `Object (new closureObject f :> _ Object.phpObject))]

