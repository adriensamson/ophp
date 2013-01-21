{
    open Parser
    
    type currentRule = Outer | Token | InString
    
    let currentRule = ref Outer
    
    let hexDecode s =
        let result = ref 0 in
        let f c = match c with
            | '0' -> 0
            | '1' -> 1
            | '2' -> 2
            | '3' -> 3
            | '4' -> 4
            | '5' -> 5
            | '6' -> 6
            | '7' -> 7
            | '8' -> 8
            | '9' -> 9
            | 'a' | 'A' -> 10
            | 'b' | 'B' -> 11
            | 'c' | 'C' -> 12
            | 'd' | 'D' -> 13
            | 'e' | 'E' -> 14
            | 'f' | 'F' -> 15
            | _ -> failwith "not hexa"
        in
        String.iter (fun c -> result := !result * 16 + f c) s;
        !result

    let octalDecode s =
        let result = ref 0 in
        let f c = match c with
            | '0' -> 0
            | '1' -> 1
            | '2' -> 2
            | '3' -> 3
            | '4' -> 4
            | '5' -> 5
            | '6' -> 6
            | '7' -> 7
            | _ -> failwith "not octal"
        in
        String.iter (fun c -> result := !result * 8 + f c) s;
        !result

    let unescapeSimpleQuotes s =
        let regexp = Str.regexp "\\\\." in
        let f s = match Str.matched_string s with
            | "\\\\" -> "\\"
            | "\\'" -> "'"
            | _ -> s
        in
        Str.global_substitute regexp f s
    
    let unescapeDoubleQuotes s =
        let regexp = Str.regexp "\\\\\\([nrtvef$]\\|[0-7][0-7]?[0-7]?\\|x[0-9A-Fa-f][0-9A-Fa-f]?\\)" in
        let f s = match Str.matched_string s with
            | "\\\\" -> "\\"
            | "\\\"" -> "\""
            | "\\n" -> "\n"
            | "\\r" -> "\r"
            | "\\t" -> "\t"
            | "\\v" -> "\x0B"
            | "\\e" -> "\x1B"
            | "\\f" -> "\x1C"
            | "\\$" -> "$"
            | ss -> match ss.[1] with
                | 'x' -> String.make 1 (char_of_int (hexDecode (String.sub ss 2 (String.length ss - 2))))
                | _ -> String.make 1 (char_of_int (octalDecode (String.sub ss 1 (String.length ss - 1))))
        in
        Str.global_substitute regexp f s
            
}
let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z' '_' '\x7f'-'\xff']['a'-'z' 'A'-'Z' '0'-'9' '_' '\x7f'-'\xff']*

rule outer = parse
    | "<?php" { currentRule := Token; token lexbuf }
    | ([^'<'] | ('<' [^'?']) | ("<?" [^'p']) | ("<?p" [^'h']) | ("<?ph" [^'p']))* as html { T_INLINE_HTML html }
    | eof { END }

and token = parse
    | [' ' '\t' '\n']	{ token lexbuf }
    | ("//"|'#') [^ '\n' ]* '\n' { token lexbuf }
    | "/*" ([^'*']|'*'[^'/'])* "*/" { token lexbuf }
    
    | "?>" { currentRule := Outer; outer lexbuf }
    | '"' { currentRule := InString; TT_DOUBLE_QUOTE }
    
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
    
    | "=>" { T_DOUBLE_ARROW }
    
    | "echo" { T_ECHO }
    | "function" { T_FUNCTION }
    | "return" { T_RETURN }
    | "null" { T_NULL }
    | "false" { T_FALSE }
    | "true" { T_TRUE }
    | "clone" { T_CLONE }
    | "new" { T_NEW }
    | "instanceof" { T_INSTANCEOF }
    | "array" { T_ARRAY }
    | "if" { T_IF }
    | "else" { T_ELSE }
    | "elseif" { T_ELSEIF }
    | "while" { T_WHILE }
    | "for" { T_FOR }
    | "foreach" { T_FOREACH }
    | "as" { T_AS }
    | "break" { T_BREAK }
    | "continue" { T_CONTINUE }
    
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
    
    | "'" (('\\' _) | [^ '\\' '\''])* "'"
        as s { TT_CONSTANT_STRING (unescapeSimpleQuotes (String.sub s 1 (String.length s - 2))) }
    
    | ident as s { T_STRING s }
    | '$' ident as s { T_VARIABLE (String.sub s 1 (String.length s - 1)) }
    
    | _ { token lexbuf }
    | eof { END }
    
and inString = parse
    | '"' { currentRule := Token; TT_DOUBLE_QUOTE }
    | ('\\' _ |[^'"' '$' '{']|'{'[^'$'])* as s { TT_CONSTANT_STRING (unescapeDoubleQuotes s) }

{
    let parse lexbuf = match !currentRule with
        | Outer -> outer lexbuf
        | Token -> token lexbuf
        | InString -> inString lexbuf
}

