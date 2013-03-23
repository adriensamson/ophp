open Interpreter

let (closureClass : (('v PhpArray.phpArray, 'v Object.phpObject)  Language.Typing.value as 'v) Object.phpClass)
    = new Object.phpClass "Closure" false false true false None [] [] [] [("__construct", Language.Typing.Private, fun _ _ _ -> failwith "Non instanciable")] [] []
class type ['v] variable = object
    method get: 'v
    method set: 'v -> unit
    end

class closureObject
    (f:(('v PhpArray.phpArray, 'v Object.phpObject) Language.Typing.value as 'v) variable list -> 'v variable)
= object (self)
    inherit [('v PhpArray.phpArray, 'v Object.phpObject)  Language.Typing.value as 'v] Object.phpObject closureClass as parent
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

