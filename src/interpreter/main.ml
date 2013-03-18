let addExtension filename =
    Extension.loadFile filename

let run chan =
    let parse chan =
        Syntax.Lexer.reset ();
        let lexbuf = Lexing.from_channel chan in
        Syntax.Parser.everything Syntax.Lexer.parse lexbuf
    in
    let filesr = new Registry.fileRegistry parse in
    let context = Expression.makeContext (new Variable.variableRegistry) (new Registry.functionRegistry) (new Registry.classRegistry) filesr in
    Extension.loadExtenstionsInContext context;
    let compiler = new Compiler.compiler in
    let cfile = compiler#compileFile (filesr#parseChannel chan) in
    cfile context
