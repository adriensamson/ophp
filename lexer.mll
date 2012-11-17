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
    | '{' { TT_LEFT_BRACE }
    | '}' { TT_RIGHT_BRACE }
    | '[' { TT_LEFT_BRACKET }
    | ']' { TT_RIGHT_BRACKET }
    | ',' { TT_COMMA }
    | '~' { TT_TILDE }
    | '@' { TT_AT }
    | '!' { TT_EXCL }
    | '%' { TT_MOD }
    | '.' { TT_CONCAT }
    | '=' { TT_EQUAL }
    | '?' { TT_INTEROGATION }
    | ':' { TT_DOUBLE_COLON }
    | '|' { TT_BITWISE_OR }
    | '^' { TT_BITWISE_XOR }
    | '&' { TT_BITWISE_AND }
    | '>' { TT_GREATER }
    | '<' { TT_SMALLER }
    
    | "++" { T_INC }
    | "--" { T_DEC }
    | "<<" { T_SL }
    | ">>" { T_SR }
    | "or" { T_LOGICAL_OR }
    | "xor" { T_LOGICAL_XOR }
    | "and" { T_LOGICAL_AND }
    | "+=" { T_PLUS_EQUAL }
    | "-=" { T_MINUS_EQUAL }
    | "*=" { T_MUL_EQUAL }
    | "/=" { T_DIV_EQUAL }
    | ".=" { T_CONCAT_EQUAL }
    | "%=" { T_MOD_EQUAL }
    | "&=" { T_AND_EQUAL }
    | "|=" { T_OR_EQUAL }
    | "^=" { T_XOR_EQUAL }
    | "<<=" { T_SL_EQUAL }
    | ">>=" { T_SR_EQUAL }
    | "||" { T_BOOLEAN_OR }
    | "&&" { T_BOOLEAN_AND }
    | "==" { T_IS_EQUAL }
    | "!=" | "<>" { T_IS_NOT_EQUAL }
    | "===" { T_IS_IDENTICAL }
    | "!==" { T_IS_NOT_IDENTICAL }
    | ">=" { T_IS_GREATER_OR_EQUAL }
    | "<=" { T_IS_SMALLER_OR_EQUAL }
    
    | "echo" { T_ECHO }
    | "function" { T_FUNCTION }
    | "return" { T_RETURN }
    | "null" { T_NULL }
    | "false" { T_FALSE }
    | "true" { T_TRUE }
    | "clone" { T_CLONE }
    | "new" { T_NEW }
    | "instanceof" { T_INSTANCEOF }
    
    | '(' ' '* ("int"|"long") ' '* ')' { T_INT_CAST }
    | '(' ' '* ("double"|"float") ' '* ')' { T_DOUBLE_CAST }
    | '(' ' '* "string" ' '* ')' { T_STRING_CAST }
    | '(' ' '* "array" ' '* ')' { T_ARRAY_CAST }
    | '(' ' '* "object" ' '* ')' { T_OBJECT_CAST }
    | '(' ' '* "bool" "ean"? ' '* ')' { T_BOOL_CAST }
    | '(' ' '* "unset" ' '* ')' { T_UNSET_CAST }
    
    | digit+ as num { T_LNUMBER (int_of_string num) }
    
    | "." digit+
    | digit+ "." digit* as num { T_DNUMBER (float_of_string num) }
    
    | ident as s { T_STRING s }
    | '$' ident as s { T_VARIABLE (String.sub s 1 (String.length s - 1)) }
    
    | _ { token lexbuf }
    | eof { END }
  
