let registeredExtensions = Hashtbl.create 10

let register
    (name : string)
    (constants: (string * Sig.value) list)
    (functions : (string * (Sig.evalContext -> Sig.variable list -> Sig.variable)) list)
    (classes : (string * Sig.phpClass) list)
    (initializers : (Sig.evalContext -> unit) list)
    =
    Hashtbl.replace registeredExtensions name (constants, functions, classes, initializers)

let loadExtenstionsInContext (context : Sig.evalContext) =
    let loadExtension name (constants, functions, classes, initializers) =
        List.iter (fun (n, v) -> context#constants#set n v) constants;
        List.iter (fun (n, f) -> context#functions#set n (new Func.baseFunction [] true f)) functions;
        List.iter (fun (n, c) -> context#classes#set n c) classes;
        List.iter (fun f -> f context) initializers
    in
    Hashtbl.iter loadExtension registeredExtensions

let loadFile filename =
    Dynlink.loadfile (Dynlink.adapt_filename filename)

