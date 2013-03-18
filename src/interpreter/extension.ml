let registeredExtensions = Hashtbl.create 10

let register
    (name : string)
    (constants: (string * (('v PhpArray.phpArray as 'a, 'v Object.phpObject as 'o) Language.Typing.value as 'v)) list)
    (functions : (string * ('v list -> 'v)) list)
    (classes : (string * ('v Object.phpClass as 'c)) list)
    (initializers : (('a, 'o, 'c) Compiler.evalContext -> unit) list)
    =
    Hashtbl.replace registeredExtensions name (constants, functions, classes, initializers)

let loadExtenstionsInContext (context : (_,_,_) Compiler.evalContext) =
    let loadExtension name (constants, functions, classes, initializers) =
        List.iter (fun (n, v) -> ()) constants;
        List.iter (fun (n, f) -> context#functions#add n f) functions;
        List.iter (fun (n, c) -> context#classes#add n c) classes;
        List.iter (fun f -> f context) initializers
    in
    Hashtbl.iter loadExtension registeredExtensions

let loadFile filename =
    Dynlink.loadfile (Dynlink.adapt_filename filename)

