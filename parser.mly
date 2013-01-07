%{
open Language
%}

%token <float> T_DNUMBER
%token <int> T_LNUMBER
%token <string> T_STRING T_VARIABLE TT_CONSTANT_STRING T_INLINE_HTML

%token TT_MUL TT_DIV TT_PLUS TT_MINUS TT_SEMI_COLON TT_LEFT_PAR TT_RIGHT_PAR TT_LEFT_BRACKET TT_RIGHT_BRACKET TT_LEFT_BRACE TT_RIGHT_BRACE TT_COMMA TT_TILDE TT_AT TT_EXCL TT_MOD TT_CONCAT TT_EQUAL TT_INTEROGATION TT_DOUBLE_COLON TT_BITWISE_OR TT_BITWISE_XOR TT_BITWISE_AND TT_GREATER TT_SMALLER

%token T_INC T_DEC T_SL T_SR T_LOGICAL_OR T_LOGICAL_XOR T_LOGICAL_AND T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL T_BOOLEAN_OR T_BOOLEAN_AND T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_IS_GREATER_OR_EQUAL T_IS_SMALLER_OR_EQUAL T_DOUBLE_ARROW

%token T_ECHO T_FUNCTION T_RETURN T_NULL T_FALSE T_TRUE T_CLONE T_NEW T_INSTANCEOF T_ARRAY
%token T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST

%token END

%left T_LOGICAL_OR
%left T_LOGICAL_XOR
%left T_LOGICAL_AND
%left TT_EQUAL T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL
%left TT_INTEROGATION TT_DOUBLE_COLON
%left T_BOOLEAN_OR
%left T_BOOLEAN_AND 
%left TT_BITWISE_OR
%left TT_BITWISE_XOR
%left TT_BITWISE_AND
%nonassoc T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL
%nonassoc TT_SMALLER T_IS_SMALLER_OR_EQUAL TT_GREATER T_IS_GREATER_OR_EQUAL
%left T_SL T_SR
%left TT_PLUS TT_MINUS TT_CONCAT
%left TT_MUL TT_DIV TT_MOD
%right TT_EXCL
%nonassoc T_INSTANCEOF
%right TT_TILDE T_INC T_DEC T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST TT_AT
%left TT_LEFT_BRACKET
%nonassoc T_CLONE T_NEW

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
    | T_INLINE_HTML  { Ast.Echo (Ast.ConstValue (`String $1)) }
    | expr TT_SEMI_COLON		{ Ast.IgnoreResult ($1) }
    | T_RETURN expr TT_SEMI_COLON       { Ast.Return ($2) }
    | T_FUNCTION T_STRING TT_LEFT_PAR argument_definition_list TT_RIGHT_PAR TT_LEFT_BRACE stmt_list TT_RIGHT_BRACE { Ast.FunctionDef ($2, $4, $7) }


argument_definition_list:
      { [] }
    | argument_definition_tail { $1 }
argument_definition_tail:
    | identifier { [$1] }
    | identifier TT_COMMA argument_definition_tail { $1::$3 }

identifier:
      T_VARIABLE { $1 }
      
argument_call_list:
      { [] }
    | argument_call_tail { $1 }
argument_call_tail:
    | expr { [$1] }
    | expr TT_COMMA argument_call_tail { $1::$3 }

array_content_list:
      { [] }
    | expr { [(Ast.ConstValue `Null, $1)] }
    | expr TT_COMMA array_content_list  { (Ast.ConstValue `Null, $1)::$3 }
    | expr T_DOUBLE_ARROW expr { [($1, $3)] }
    | expr T_DOUBLE_ARROW expr TT_COMMA array_content_list { ($1, $3)::$5 }

assignable:
    | T_VARIABLE { Ast.Variable $1 }
    | assignable TT_LEFT_BRACKET expr TT_RIGHT_BRACKET { Ast.ArrayOffset ($1, Some $3) }
    | assignable TT_LEFT_BRACKET TT_RIGHT_BRACKET { Ast.ArrayOffset ($1, None) }

expr:
      T_DNUMBER { Ast.ConstValue (`Double $1) }
    | T_LNUMBER { Ast.ConstValue (`Long $1) }
    | T_NULL { Ast.ConstValue (`Null) }
    | T_FALSE { Ast.ConstValue (`Bool false) }
    | T_TRUE { Ast.ConstValue (`Bool true) }
    | TT_CONSTANT_STRING { Ast.ConstValue (`String $1) }
    | TT_LEFT_PAR expr TT_RIGHT_PAR { $2 }
    | expr TT_PLUS expr { Ast.BinaryOperation(Ast.Plus, $1, $3) }
    | expr TT_MINUS expr { Ast.BinaryOperation(Ast.Minus, $1, $3) }
    | expr TT_MUL expr { Ast.BinaryOperation(Ast.Mult, $1, $3) }
    | expr TT_DIV expr { Ast.BinaryOperation(Ast.Div, $1, $3) }
    | expr TT_MOD expr { Ast.BinaryOperation(Ast.Modulo, $1, $3) }
    | expr TT_CONCAT expr { Ast.BinaryOperation(Ast.Concat, $1, $3) }
    | expr T_BOOLEAN_AND expr { Ast.And ($1, $3) }
    | expr T_LOGICAL_AND expr { Ast.And ($1, $3) }
    | expr T_BOOLEAN_OR expr { Ast.Or ($1, $3) }
    | expr T_LOGICAL_OR expr { Ast.Or ($1, $3) }
    | expr T_LOGICAL_XOR expr { Ast.Xor ($1, $3) }
    | expr TT_BITWISE_AND expr { Ast.BinaryOperation(Ast.BitwiseAnd, $1, $3) }
    | expr TT_BITWISE_OR expr { Ast.BinaryOperation(Ast.BitwiseOr, $1, $3) }
    | expr TT_BITWISE_XOR expr { Ast.BinaryOperation(Ast.BitwiseXor, $1, $3) }
    | expr T_SL expr { Ast.BinaryOperation(Ast.ShiftLeft, $1, $3) }
    | expr T_SR expr { Ast.BinaryOperation(Ast.ShiftRight, $1, $3) }
    | TT_EXCL expr { Ast.Not ($2) }
    | assignable TT_EQUAL expr { Ast.Assign ($1, $3) }
    | assignable T_PLUS_EQUAL expr { Ast.BinaryAssign (Ast.Plus, $1, $3) }
    | assignable T_MINUS_EQUAL expr { Ast.BinaryAssign (Ast.Minus, $1, $3) }
    | assignable T_MUL_EQUAL expr { Ast.BinaryAssign (Ast.Mult, $1, $3) }
    | assignable T_DIV_EQUAL expr { Ast.BinaryAssign (Ast.Div, $1, $3) }
    | assignable T_MOD_EQUAL expr { Ast.BinaryAssign (Ast.Modulo, $1, $3) }
    | assignable T_CONCAT_EQUAL expr { Ast.BinaryAssign (Ast.Concat, $1, $3) }
    | assignable T_AND_EQUAL expr { Ast.BinaryAssign (Ast.BitwiseAnd, $1, $3) }
    | assignable T_OR_EQUAL expr { Ast.BinaryAssign (Ast.BitwiseOr, $1, $3) }
    | assignable T_XOR_EQUAL expr { Ast.BinaryAssign (Ast.BitwiseXor, $1, $3) }
    | assignable T_SL_EQUAL expr { Ast.BinaryAssign (Ast.ShiftLeft, $1, $3) }
    | assignable T_SR_EQUAL expr { Ast.BinaryAssign (Ast.ShiftRight, $1, $3) }
    | T_STRING TT_LEFT_PAR argument_call_list TT_RIGHT_PAR { Ast.FunctionCall ($1, $3) }
    | T_ARRAY TT_LEFT_PAR array_content_list TT_RIGHT_PAR { Ast.ArrayConstructor $3 }
    | assignable { Ast.Assignable $1 }
;
%%
