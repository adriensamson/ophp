%{
open Language
%}

%token <float> T_DNUMBER
%token <int> T_LNUMBER
%token <string> T_STRING T_VARIABLE
%token TT_MUL TT_DIV TT_PLUS TT_MINUS TT_SEMI_COLON TT_LEFT_PAR TT_RIGHT_PAR TT_LEFT_BRACKET TT_RIGHT_BRACKET TT_COMMA
%token T_ECHO T_FUNCTION
%token END

%left PLUS MINUS
%left MULTIPLY DIVIDE

%start everything
%type <Language.Ast.stmt list> everything

%% /* Grammar rules and actions follow */

everything:
      stmt_list END { $1 }

stmt_list:
      { [] }
    | stmt stmt_list { $1::$2 }

stmt:
      T_ECHO expr TT_SEMI_COLON		{ Ast.Echo ($2) }
    | expr TT_SEMI_COLON		{ Ast.IgnoreResult ($1) }
    | T_FUNCTION T_STRING TT_LEFT_PAR argument_list_definition TT_RIGHT_PAR TT_LEFT_BRACKET stmt_list TT_RIGHT_BRACKET { Ast.FunctionDef ($2, $4, $7) }


argument_list_definition:
      { [] }
    | identifier { [$1] }
    | identifier TT_COMMA argument_list_definition { $1::$3 }

identifier:
      T_VARIABLE { $1 }
      
argument_list_call:
      { [] }
    | expr { [$1] }
    | expr TT_COMMA argument_list_call { $1::$3 }

expr:
      T_DNUMBER { Ast.ConstValue (Typing.Double $1) }
    | T_LNUMBER { Ast.ConstValue (Typing.Long $1) }
    | T_VARIABLE { Ast.Variable $1 }
    | expr TT_PLUS expr { Ast.Plus ($1, $3) }
    | expr TT_MINUS expr { Ast.Minus ($1, $3) }
    | expr TT_MUL expr { Ast.Mult ($1, $3) }
    | expr TT_DIV expr { Ast.Div ($1, $3) }
    | T_STRING TT_LEFT_PAR argument_list_call TT_RIGHT_PAR { Ast.FunctionCall ($1, $3) }
;
%%
