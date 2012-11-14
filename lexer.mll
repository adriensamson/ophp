{
  open Parser
}
let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z' '_' '\x7f'-'\xff']['a'-'z' 'A'-'Z' '0'-'9' '_' '\x7f'-'\xff']*
rule token = parse
    | [' ' '\t' '\n']	{ token lexbuf }
    | ';' { TT_SEMI_COLON }
    | '+' { TT_PLUS }
    | '-' { TT_MINUS }
    | '*' { TT_MUL }
    | '/' { TT_DIV }
    | '(' { TT_LEFT_PAR }
    | ')' { TT_RIGHT_PAR }
    | '{' { TT_LEFT_BRACKET }
    | '}' { TT_RIGHT_BRACKET }
    | ',' { TT_COMMA }
    
    | "echo" { T_ECHO }
    | "function" { T_FUNCTION }
    
    | digit+ as num { T_LNUMBER (int_of_string num) }
    
    | "." digit+
    | digit+ "." digit* as num { T_DNUMBER (float_of_string num) }
    
    | ident as s { T_STRING s }
    
    | _ { token lexbuf }
    | eof { END }
  
