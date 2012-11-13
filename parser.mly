%{
%}

%token <float> NUM
%token MULTIPLY DIVIDE PLUS MINUS NEWLINE ECHO END
%left PLUS MINUS
%left MULTIPLY DIVIDE



%start stmt
%type <Ast.stmt> stmt

%% /* Grammar rules and actions follow */
stmt:
      NEWLINE   { Ast.Nothing }
    | ECHO expr NEWLINE		{ Ast.Echo ($2) }
;

expr:      NUM { Ast.Num $1 }
        | expr PLUS expr { Ast.Plus ($1, $3) }
        | expr MINUS expr { Ast.Minus ($1, $3) }
        | expr MULTIPLY expr { Ast.Mult ($1, $3) }
        | expr DIVIDE expr { Ast.Div ($1, $3) }
;
%%
