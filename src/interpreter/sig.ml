type value = (phpArray, phpObject) Language.Typing.value
and phpArray = value PhpArray.phpArray
and phpObject = value Object.phpObject
and phpClass = value Object.phpClass

type variable = value Language.Typing.variable

class type obj = object end

class type variableRegistry
= object
    method replace : string -> variable -> unit
    method find : string -> variable
    method replaceSuperglobal : string -> variable -> unit
    method findSuperglobal : string -> variable
    method newScope : obj -> variableRegistry
    method addFromParent : ?byRef:bool -> string -> unit
    method addFromGlobal : string -> unit
    method addFromStatic : string -> value -> unit
end


class type evalContext = object
    method vars : variableRegistry
    method obj : phpObject option
    method callingClass : phpClass option
    method staticClass : phpClass option
    method namespace : string list
    method resolveNamespace : ?fallbackTest:(string -> bool) -> Language.Ast.name -> string
    method constants : value Registry.constantRegistry
    method functions : (phpArray, phpObject, phpClass, evalContext) Func.baseFunction Registry.functionRegistry
    method classes : phpClass Registry.classRegistry
    method files : value Registry.fileRegistry
    method functionScope : ?callingClass:phpClass -> ?obj:phpObject -> ?staticClass:phpClass -> obj -> evalContext
    method namespaceScope : namespace:(string list) -> namespaceUses:((string list * string option) list) -> unit -> evalContext
    method newClosure : (variable list -> variable) -> value
    method setClosureFactory : ((variable list -> variable) -> value) -> unit
end
