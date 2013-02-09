%{
open Language

let rec make_if_else cond then_list elseifs else_list = match elseifs with
    | [] -> Ast.IfElse (cond, then_list, else_list)
    | (c, tl)::t -> Ast.IfElse (cond, then_list, [make_if_else c tl t else_list])

let rec make_if cond then_list elseifs = match elseifs with
    | [] -> Ast.If (cond, then_list)
    | (c, tl)::t -> Ast.IfElse (cond, then_list, [make_if c tl t])
%}

%token <float> T_DNUMBER
%token <int> T_LNUMBER
%token <string> T_STRING T_VARIABLE TT_CONSTANT_STRING T_INLINE_HTML TT_VARIABLE_VARIABLE

%token TT_MUL TT_DIV TT_PLUS TT_MINUS TT_SEMI_COLON TT_LEFT_PAR TT_RIGHT_PAR TT_LEFT_BRACKET TT_RIGHT_BRACKET TT_LEFT_BRACE TT_RIGHT_BRACE TT_COMMA TT_TILDE TT_AT TT_EXCL TT_MOD TT_CONCAT TT_EQUAL TT_INTEROGATION TT_DOUBLE_COLON TT_BITWISE_OR TT_BITWISE_XOR TT_BITWISE_AND TT_GREATER TT_SMALLER TT_DOUBLE_QUOTE

%token T_INC T_DEC T_SL T_SR T_LOGICAL_OR T_LOGICAL_XOR T_LOGICAL_AND T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL T_BOOLEAN_OR T_BOOLEAN_AND T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_IS_GREATER_OR_EQUAL T_IS_SMALLER_OR_EQUAL T_DOUBLE_ARROW T_OBJECT_OPERATOR T_CURLY_OPEN T_DOLLAR_OPEN_CURLY_BRACES T_DOUBLE_COLON

%token T_ECHO T_FUNCTION T_RETURN T_NULL T_FALSE T_TRUE T_CLONE T_NEW T_INSTANCEOF T_ARRAY T_CLASS T_EXTENDS T_ABSTRACT T_FINAL T_STATIC T_IMPLEMENTS T_INTERFACE T_PUBLIC T_PROTECTED T_PRIVATE T_PARENT T_SELF T_THIS T_CONST T_INCLUDE T_INCLUDE_ONCE T_REQUIRE T_REQUIRE_ONCE
%token T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST
%token T_IF T_ELSE T_ELSEIF T_WHILE T_FOR T_FOREACH T_AS T_BREAK T_CONTINUE

%token END

%nonassoc T_INCLUDE T_INCLUDE_ONCE T_REQUIRE T_REQUIRE_ONCE
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
    
control_stmt_list:
    | TT_SEMI_COLON { [] }
    | control_stmt { [$1] }
    | TT_LEFT_BRACE stmt_list TT_RIGHT_BRACE { $2 }

any_control_stmt_list:
    | control_stmt_list { $1 }
    | incomplete_if_stmt { [$1] }

elseif:
    | T_ELSEIF TT_LEFT_PAR expr TT_RIGHT_PAR control_stmt_list { ($3, $5) }

elseifs:
      { [] }
    | elseifs elseif { $1 @ [$2] }

incomplete_elseif:
    | T_ELSEIF TT_LEFT_PAR expr TT_RIGHT_PAR incomplete_if_stmt { ($3, [$5]) }

control_stmt:
      T_ECHO expr TT_SEMI_COLON		{ Ast.Echo ($2) }
    | T_INLINE_HTML  { Ast.Echo (Ast.ConstValue (`String $1)) }
    | expr TT_SEMI_COLON		{ Ast.IgnoreResult ($1) }
    | T_RETURN expr TT_SEMI_COLON       { Ast.Return ($2) }
    | T_FUNCTION T_STRING TT_LEFT_PAR argument_definition_list TT_RIGHT_PAR TT_LEFT_BRACE stmt_list TT_RIGHT_BRACE { Ast.FunctionDef ($2, $4, $7) }
    | T_IF TT_LEFT_PAR expr TT_RIGHT_PAR control_stmt_list elseifs T_ELSE control_stmt_list { make_if_else $3 $5 $6 $8 }
    | T_WHILE TT_LEFT_PAR expr TT_RIGHT_PAR control_stmt_list { Ast.While ($3, $5) }
    | T_FOR TT_LEFT_PAR argument_call_list TT_SEMI_COLON argument_call_list TT_SEMI_COLON argument_call_list TT_RIGHT_PAR control_stmt_list { Ast.For ($3, $5, $7, $9) }
    | T_FOREACH TT_LEFT_PAR expr T_AS T_VARIABLE T_DOUBLE_ARROW T_VARIABLE TT_RIGHT_PAR control_stmt_list { Ast.Foreach ($3, Some $5, $7, $9) }
    | T_FOREACH TT_LEFT_PAR expr T_AS T_VARIABLE TT_RIGHT_PAR control_stmt_list { Ast.Foreach ($3, None, $5, $7) }
    | T_BREAK TT_SEMI_COLON { Ast.Break 1 }
    | T_BREAK T_LNUMBER TT_SEMI_COLON { Ast.Break $2 }
    | T_CONTINUE TT_SEMI_COLON { Ast.Continue 1 }
    | T_CONTINUE T_LNUMBER TT_SEMI_COLON { Ast.Continue $2 }
    | T_CLASS T_STRING TT_LEFT_BRACE class_def_content_list TT_RIGHT_BRACE { Ast.ClassDef ($2, false, false, false, false, None, [], $4) }
    | T_CLASS T_STRING T_EXTENDS T_STRING TT_LEFT_BRACE class_def_content_list TT_RIGHT_BRACE { Ast.ClassDef ($2, false, false, false, false, Some $4, [], $6) }

stmt:
    | control_stmt { $1 }
    | incomplete_if_stmt { $1 }
    
incomplete_if_stmt:
    | T_IF TT_LEFT_PAR expr TT_RIGHT_PAR control_stmt_list elseifs T_ELSE incomplete_if_stmt { make_if_else $3 $5 $6 [$8] }
    | T_IF TT_LEFT_PAR expr TT_RIGHT_PAR control_stmt_list elseifs elseif { make_if $3 $5 ($6 @ [$7]) }
    | T_IF TT_LEFT_PAR expr TT_RIGHT_PAR control_stmt_list elseifs incomplete_elseif { make_if $3 $5 ($6 @ [$7]) }
    | T_IF TT_LEFT_PAR expr TT_RIGHT_PAR any_control_stmt_list { make_if $3 $5 [] }

visibility:
    | T_PUBLIC { Typing.Public }
    | T_PROTECTED { Typing.Protected }
    | T_PRIVATE { Typing.Private }

class_def_content_list:
      { [] }
    | class_def_content class_def_content_list { $1::$2 }

class_def_content:
    | T_CONST T_STRING TT_EQUAL expr TT_SEMI_COLON { Ast.ConstantDef ($2, $4) }
    | visibility T_VARIABLE TT_SEMI_COLON { Ast.PropertyDef ($2, false, $1, None) }
    | visibility T_STATIC T_VARIABLE TT_SEMI_COLON { Ast.PropertyDef ($3, true, $1, None) }
    | visibility T_VARIABLE TT_EQUAL expr TT_SEMI_COLON { Ast.PropertyDef ($2, false, $1, Some $4) }
    | visibility T_STATIC T_VARIABLE TT_EQUAL expr TT_SEMI_COLON { Ast.PropertyDef ($3, true, $1, Some $5) }
    | visibility T_FUNCTION T_STRING TT_LEFT_PAR argument_definition_list TT_RIGHT_PAR TT_LEFT_BRACE stmt_list TT_RIGHT_BRACE { Ast.MethodDef ($3, false, $1, $5, $8) }
    | visibility T_STATIC T_FUNCTION T_STRING TT_LEFT_PAR argument_definition_list TT_RIGHT_PAR TT_LEFT_BRACE stmt_list TT_RIGHT_BRACE { Ast.MethodDef ($4, true, $1, $6, $9) }

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
    | TT_VARIABLE_VARIABLE { Ast.VariableVariable (Ast.Assignable (Ast.Variable $1)) }
    | T_DOLLAR_OPEN_CURLY_BRACES expr TT_RIGHT_BRACE { Ast.VariableVariable $2 }
    | class_reference T_DOUBLE_COLON T_VARIABLE { Ast.StaticProperty ($1, $3) }
    | fluent TT_LEFT_BRACKET expr TT_RIGHT_BRACKET { Ast.ArrayOffset ($1, Some $3) }
    | fluent TT_LEFT_BRACKET TT_RIGHT_BRACKET { Ast.ArrayOffset ($1, None) }
    | fluent T_OBJECT_OPERATOR T_STRING { Ast.Property ($1, $3) }
    | T_THIS T_OBJECT_OPERATOR T_STRING { Ast.Property (Ast.This, $3) }

fluent:
    | assignable { Ast.Assignable $1 }
    | fluent T_OBJECT_OPERATOR T_STRING TT_LEFT_PAR argument_call_list TT_RIGHT_PAR { Ast.MethodCall ($1, $3, $5) }
    | T_THIS T_OBJECT_OPERATOR T_STRING TT_LEFT_PAR argument_call_list TT_RIGHT_PAR { Ast.MethodCall (Ast.This, $3, $5) }

double_quoted_content_list:
      { [] }
    | TT_CONSTANT_STRING double_quoted_content_list { Ast.ConstValue (`String $1)::$2 }
    | assignable double_quoted_content_list { (Ast.Assignable $1)::$2 }
    | T_CURLY_OPEN assignable TT_RIGHT_BRACE double_quoted_content_list { (Ast.Assignable $2)::$4 }

expr:
      T_DNUMBER { Ast.ConstValue (`Double $1) }
    | T_LNUMBER { Ast.ConstValue (`Long $1) }
    | T_NULL { Ast.ConstValue (`Null) }
    | T_FALSE { Ast.ConstValue (`Bool false) }
    | T_TRUE { Ast.ConstValue (`Bool true) }
    | TT_CONSTANT_STRING { Ast.ConstValue (`String $1) }
    | TT_DOUBLE_QUOTE double_quoted_content_list TT_DOUBLE_QUOTE { Ast.ConcatList $2 }
    
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
    | expr T_IS_GREATER_OR_EQUAL expr { Ast.Comparison (Ast.GreaterEqual, $1, $3) }
    | expr TT_GREATER expr { Ast.Comparison (Ast.Greater, $1, $3) }
    | expr TT_SMALLER expr { Ast.Comparison (Ast.Lesser, $1, $3) }
    | expr T_IS_SMALLER_OR_EQUAL expr { Ast.Comparison (Ast.LesserEqual, $1, $3) }
    | expr T_IS_EQUAL expr { Ast.Comparison (Ast.Equal, $1, $3) }
    | expr T_IS_IDENTICAL expr { Ast.Comparison (Ast.Identical, $1, $3) }
    | expr T_IS_NOT_EQUAL expr { Ast.Comparison (Ast.NotEqual, $1, $3) }
    | expr T_IS_NOT_IDENTICAL expr { Ast.Comparison (Ast.NotIdentical, $1, $3) }
    
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
    
    | T_INC assignable { Ast.PreInc $2 }
    | assignable T_INC { Ast.PostInc $1 }
    | T_DEC assignable { Ast.PreDec $2 }
    | assignable T_DEC { Ast.PostDec $1 }
    
    | T_STRING TT_LEFT_PAR argument_call_list TT_RIGHT_PAR { Ast.FunctionCall ($1, $3) }
    | T_ARRAY TT_LEFT_PAR array_content_list TT_RIGHT_PAR { Ast.ArrayConstructor $3 }
    | T_NEW T_STRING TT_LEFT_PAR argument_call_list TT_RIGHT_PAR { Ast.NewObject ($2, $4) }
    | class_reference T_DOUBLE_COLON T_STRING TT_LEFT_PAR argument_call_list TT_RIGHT_PAR { Ast.StaticMethodCall ($1, $3, $5) }
    | class_reference T_DOUBLE_COLON T_STRING { Ast.ClassConstant ($1, $3) }
    
    | T_INCLUDE expr { Ast.Include ($2, false, false) }
    | T_INCLUDE_ONCE expr { Ast.Include ($2, false, true) }
    | T_REQUIRE expr { Ast.Include ($2, true, false) }
    | T_REQUIRE_ONCE expr { Ast.Include ($2, true, true) }
    
    | fluent { $1 }
    
class_reference:
    | T_STRING { Ast.ClassName $1 }
    | T_SELF { Ast.Self }
    | T_STATIC { Ast.Static }
    | T_PARENT { Ast.Parent }
;
%%
