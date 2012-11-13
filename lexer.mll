{
  open Parser
}
let digit = ['0'-'9']
rule token = parse
  | [' ' '\t']	{ token lexbuf }
  | '\n'	{ NEWLINE }
  | "echo" { ECHO }
  | digit+
  | "." digit+
  | digit+ "." digit* as num
		{ NUM (float_of_string num) }
  | '+'		{ PLUS }
  | '-'		{ MINUS }
  | '*'		{ MULTIPLY }
  | '/'		{ DIVIDE }
  | _		{ token lexbuf }
  | eof		{ raise End_of_file }
  
