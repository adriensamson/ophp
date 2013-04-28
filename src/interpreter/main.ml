let addExtension filename =
    Extension.loadFile filename

let run filename =
    let chan =
        if filename = "" then
            stdin
        else
            try
                let dirsep = String.rindex filename '/' in
                Unix.chdir (String.sub filename 0 dirsep);
                print_endline (Unix.getcwd ());
                open_in (String.sub filename (dirsep+1) ((String.length filename) - dirsep - 1))
            with
            | Not_found -> open_in filename
    in
    let parse chan =
        begin try
            if input_char chan = '#' && input_char chan = '!' then
                let _ = input_line chan in ()
            else
                seek_in chan 0;
        with
        | End_of_file -> seek_in chan 0
        end;
        Syntax.Lexer.reset ();
        let lexbuf = Lexing.from_channel chan in
        Syntax.Parser.everything Syntax.Lexer.parse lexbuf
    in
    let filesr = new Registry.fileRegistry parse in
    let context = Expression.makeContext (new Registry.constantRegistry) (new Variable.variableRegistry) (new Registry.functionRegistry) (new Registry.classRegistry) filesr in
    Extension.loadExtenstionsInContext context;
    let compiler = new Compiler.compiler in
    let compileContext = (new Compiler.compileContext)#setFile filename in
    let cfile = compiler#compileFile compileContext (filesr#parseChannel chan) in
    cfile context

