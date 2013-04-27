open Language.Typing
open Interpreter

let autoload = object (self)
    val mutable autoloaders = []
    method addAutoloader c a =
        autoloaders <- autoloaders @ [(c, a)];
        c#classes#setAutoload self#autoload
    method autoload name =
        let found = ref false in
        let autoloaders = ref autoloaders in
        while not !found && !autoloaders <> [] do
            let (context, a) = List.hd !autoloaders in
            ignore (Compiler.invoke_callable a#get context [new variable (`String name)]);
            print_endline name;
            found := context#classes#has name;
            autoloaders := List.tl !autoloaders
        done
    end

let _ = Extension.register
    "spl"
    []
    [("spl_autoload_register", fun c args -> autoload#addAutoloader c (List.hd args); new variable (`Null))]
    []
    []

