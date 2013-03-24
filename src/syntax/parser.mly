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

%token TT_MUL TT_DIV TT_PLUS TT_MINUS TT_SEMI_COLON TT_LEFT_PAR TT_RIGHT_PAR TT_LEFT_BRACKET TT_RIGHT_BRACKET TT_LEFT_BRACE TT_RIGHT_BRACE TT_COMMA TT_TILDE TT_AT TT_EXCL TT_MOD TT_CONCAT TT_EQUAL TT_INTEROGATION TT_COLON TT_BITWISE_OR TT_BITWISE_XOR TT_BITWISE_AND TT_GREATER TT_SMALLER TT_DOUBLE_QUOTE TT_BACKSLASH

%token T_INC T_DEC T_SL T_SR T_LOGICAL_OR T_LOGICAL_XOR T_LOGICAL_AND T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL T_BOOLEAN_OR T_BOOLEAN_AND T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_IS_GREATER_OR_EQUAL T_IS_SMALLER_OR_EQUAL T_DOUBLE_ARROW T_OBJECT_OPERATOR T_CURLY_OPEN T_DOLLAR_OPEN_CURLY_BRACES T_DOUBLE_COLON

%token T_ECHO T_FUNCTION T_GLOBAL T_RETURN T_NULL T_FALSE T_TRUE T_CLONE T_NEW T_INSTANCEOF T_ARRAY T_CLASS T_EXTENDS T_ABSTRACT T_FINAL T_STATIC T_IMPLEMENTS T_INTERFACE T_PUBLIC T_PROTECTED T_PRIVATE T_PARENT T_SELF T_THIS T_CONST T_INCLUDE T_INCLUDE_ONCE T_REQUIRE T_REQUIRE_ONCE T_NAMESPACE T_USE
%token T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST
%token T_IF T_ELSE T_ELSEIF T_WHILE T_FOR T_FOREACH T_AS T_BREAK T_CONTINUE T_THROW T_TRY T_CATCH

%token END

%nonassoc T_INCLUDE T_INCLUDE_ONCE T_REQUIRE T_REQUIRE_ONCE
%left T_LOGICAL_OR
%left T_LOGICAL_XOR
%left T_LOGICAL_AND
%left TT_EQUAL T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL
%left TT_INTEROGATION TT_COLON
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
%nonassoc UNARY_MINUS
%right TT_EXCL
%nonassoc T_INSTANCEOF
%right TT_TILDE T_INC T_DEC T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST TT_AT
%left TT_LEFT_BRACKET
%nonassoc T_CLONE T_NEW

%start everything
%type <Language.Ast.namespaceStmt list> everything

%% /* Grammar rules and actions follow */

everything:
    | stmt_list END { [Ast.NamespaceBlock ([], [], $1)] }
    | namespace_blocks END { $1 }
    | namespace_blocks2 END { $1 }

namespace_blocks:
    | namespace_block { [$1] }
    | namespace_block namespace_blocks { $1::$2 }

namespace_block:
    | T_NAMESPACE namespace_name TT_SEMI_COLON use_namespace_list stmt_list { Ast.NamespaceBlock ($2, $4, $5) }

namespace_blocks2:
    | namespace_block2 { [$1] }
    | namespace_block2 namespace_blocks2 { $1::$2 }

namespace_block2:
    | T_NAMESPACE namespace_name TT_LEFT_BRACE use_namespace_list stmt_list TT_RIGHT_BRACE { Ast.NamespaceBlock ($2, $4, $5) }


namespace_name:
    | { [] }
    | namespace_parts { $1 }

namespace_parts:
    | T_STRING { [$1] }
    | T_STRING TT_BACKSLASH namespace_parts { $1::$3 }

use_namespace_list:
    | { [] }
    | use_namespace use_namespace_list { $1::$2 }
use_namespace:
    | T_USE namespace_name TT_SEMI_COLON { ($2, None) }
    | T_USE namespace_name T_AS T_STRING TT_SEMI_COLON { ($2, Some $4) }

namespaced_identifier:
    | namespace_parts { match List.rev $1 with [] -> assert false | name::parts -> Ast.RelativeName (List.rev parts, name) }
    | TT_BACKSLASH namespace_parts { match List.rev $2 with [] -> assert false | name::parts ->  Ast.FullyQualifiedName (List.rev parts, name) }

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
    | T_GLOBAL T_VARIABLE TT_SEMI_COLON { Ast.Global $2 }
    | T_ECHO expr TT_SEMI_COLON		{ Ast.Echo ($2) }
    | T_INLINE_HTML  { Ast.Echo (Ast.ConstValue (`String $1)) }
    | expr TT_SEMI_COLON		{ Ast.IgnoreResult ($1) }
    | T_RETURN expr TT_SEMI_COLON       { Ast.Return ($2) }
    | T_FUNCTION T_STRING TT_LEFT_PAR argument_definition_list TT_RIGHT_PAR TT_LEFT_BRACE stmt_list TT_RIGHT_BRACE { Ast.FunctionDef ($2, false, $4, $7) }
    | T_FUNCTION TT_BITWISE_AND T_STRING TT_LEFT_PAR argument_definition_list TT_RIGHT_PAR TT_LEFT_BRACE stmt_list TT_RIGHT_BRACE { Ast.FunctionDef ($3, true, $5, $8) }
    | T_IF TT_LEFT_PAR expr TT_RIGHT_PAR control_stmt_list elseifs T_ELSE control_stmt_list { make_if_else $3 $5 $6 $8 }
    | T_WHILE TT_LEFT_PAR expr TT_RIGHT_PAR control_stmt_list { Ast.While ($3, $5) }
    | T_FOR TT_LEFT_PAR argument_call_list TT_SEMI_COLON argument_call_list TT_SEMI_COLON argument_call_list TT_RIGHT_PAR control_stmt_list { Ast.For ($3, $5, $7, $9) }
    | T_FOREACH TT_LEFT_PAR expr T_AS T_VARIABLE T_DOUBLE_ARROW T_VARIABLE TT_RIGHT_PAR control_stmt_list { Ast.Foreach ($3, Some $5, $7, $9) }
    | T_FOREACH TT_LEFT_PAR expr T_AS T_VARIABLE TT_RIGHT_PAR control_stmt_list { Ast.Foreach ($3, None, $5, $7) }
    | T_BREAK TT_SEMI_COLON { Ast.Break 1 }
    | T_BREAK T_LNUMBER TT_SEMI_COLON { Ast.Break $2 }
    | T_CONTINUE TT_SEMI_COLON { Ast.Continue 1 }
    | T_CONTINUE T_LNUMBER TT_SEMI_COLON { Ast.Continue $2 }
    | T_THROW expr TT_SEMI_COLON { Ast.Throw $2 }
    | class_modifiers T_CLASS T_STRING TT_LEFT_BRACE class_def_content_list TT_RIGHT_BRACE {
        let (isStatic, isAbstract, isFinal) = $1 in
        Ast.ClassDef ($3, isStatic, isAbstract, isFinal, false, None, [], $5)
        }
    | class_modifiers T_CLASS T_STRING T_EXTENDS T_STRING TT_LEFT_BRACE class_def_content_list TT_RIGHT_BRACE {
        let (isStatic, isAbstract, isFinal) = $1 in
        Ast.ClassDef ($3, isStatic, isAbstract, isFinal, false, Some $5, [], $7)
        }
    | T_TRY TT_LEFT_BRACE stmt_list TT_RIGHT_BRACE catches { Ast.TryCatch ($3, $5) }

catches:
    | catch { [$1] }
    | catch catches { $1::$2 }
catch:
    | T_CATCH TT_LEFT_PAR namespaced_identifier T_VARIABLE TT_RIGHT_PAR TT_LEFT_BRACE stmt_list TT_RIGHT_BRACE { ($3, $4, $7) }

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

class_modifiers:
    | { (false, false, false) }
    | T_STATIC { (true, false, false) }
    | T_ABSTRACT { (false, true, false) }
    | T_STATIC T_ABSTRACT { (true, true, false) }
    | T_ABSTRACT T_STATIC { (true, true, false) }
    | T_FINAL { (false, false, true) }
    | T_FINAL T_STATIC { (true, true, false) }
    | T_STATIC T_FINAL { (true, false, true) }

class_content_modifiers:
    | visibility { ($1, false) }
    | visibility T_STATIC { ($1, true) }
    | T_STATIC visibility { ($2, true) }

class_def_content_list:
      { [] }
    | class_def_content class_def_content_list { $1::$2 }

class_def_content:
    | T_CONST T_STRING TT_EQUAL expr TT_SEMI_COLON { Ast.ConstantDef ($2, $4) }
    | class_content_modifiers T_VARIABLE TT_SEMI_COLON { let vis, isStatic = $1 in Ast.PropertyDef ($2, isStatic, vis, None) }
    | class_content_modifiers T_VARIABLE TT_EQUAL expr TT_SEMI_COLON { let vis, isStatic = $1 in Ast.PropertyDef ($2, isStatic, vis, Some $4) }
    | class_content_modifiers T_FUNCTION T_STRING TT_LEFT_PAR argument_definition_list TT_RIGHT_PAR TT_LEFT_BRACE stmt_list TT_RIGHT_BRACE
        { let vis, isStatic = $1 in Ast.MethodDef ($3, isStatic, false, vis, $5, $8) }
    | class_content_modifiers T_FUNCTION TT_BITWISE_AND T_STRING TT_LEFT_PAR argument_definition_list TT_RIGHT_PAR TT_LEFT_BRACE stmt_list TT_RIGHT_BRACE
        { let vis, isStatic = $1 in Ast.MethodDef ($4, isStatic, true, vis, $6, $9) }

is_reference:
    | { false }
    | TT_BITWISE_AND { true }
default_value:
    | { None }
    | TT_EQUAL expr { Some $2 }

argument_definition_list:
      { [] }
    | argument_definition_tail { $1 }
argument_definition_tail:
    | type_hint is_reference identifier default_value { [($3, $2, $1, $4)] }
    | type_hint is_reference identifier default_value TT_COMMA argument_definition_tail { ($3, $2, $1, $4)::$6 }

type_hint:
    | { Ast.NoTypeHint }
    | T_ARRAY { Ast.ArrayTypeHint }
    | namespaced_identifier { Ast.ClassTypeHint $1 }

closure_use_list:
      { [] }
    | closure_use_tail { $1 }
closure_use_tail:
    | is_reference identifier { [($2, $1)] }
    | is_reference identifier TT_COMMA closure_use_tail { ($2, $1)::$4 }

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
    | expr { [(None, $1)] }
    | expr TT_COMMA array_content_list  { (None, $1)::$3 }
    | expr T_DOUBLE_ARROW expr { [(Some $1, $3)] }
    | expr T_DOUBLE_ARROW expr TT_COMMA array_content_list { (Some $1, $3)::$5 }

assignable:
    | T_VARIABLE { Ast.Variable $1 }
    | TT_VARIABLE_VARIABLE { Ast.VariableVariable (Ast.Assignable (Ast.Variable $1)) }
    | T_DOLLAR_OPEN_CURLY_BRACES expr TT_RIGHT_BRACE { Ast.VariableVariable $2 }
    | class_reference T_DOUBLE_COLON T_VARIABLE { Ast.StaticProperty ($1, $3) }
    | fluent TT_LEFT_BRACKET expr TT_RIGHT_BRACKET { Ast.ArrayOffset ($1, Some $3) }
    | fluent TT_LEFT_BRACKET TT_RIGHT_BRACKET { Ast.ArrayOffset ($1, None) }
    | fluent T_OBJECT_OPERATOR T_STRING { Ast.Property ($1, $3) }

fluent:
    | assignable { Ast.Assignable $1 }
    | fluent T_OBJECT_OPERATOR T_STRING TT_LEFT_PAR argument_call_list TT_RIGHT_PAR { Ast.MethodCall ($1, $3, $5) }
    | T_THIS { Ast.This }

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
    | TT_MINUS expr %prec UNARY_MINUS { Ast.UnaryMinus $2 }
    | TT_LEFT_PAR expr TT_RIGHT_PAR { $2 }
    | namespaced_identifier { Ast.Constant $1 }
    
    | T_FUNCTION TT_LEFT_PAR argument_definition_list TT_RIGHT_PAR TT_LEFT_BRACE stmt_list TT_RIGHT_BRACE
        { Ast.Closure (false, $3, [], $6) }
    | T_FUNCTION TT_LEFT_PAR argument_definition_list TT_RIGHT_PAR T_USE TT_LEFT_PAR closure_use_list TT_RIGHT_PAR TT_LEFT_BRACE stmt_list TT_RIGHT_BRACE
        { Ast.Closure (false, $3, $7, $10) }
    | T_FUNCTION TT_BITWISE_AND TT_LEFT_PAR argument_definition_list TT_RIGHT_PAR TT_LEFT_BRACE stmt_list TT_RIGHT_BRACE
        { Ast.Closure (true, $4, [], $7) }
    | T_FUNCTION TT_BITWISE_AND TT_LEFT_PAR argument_definition_list TT_RIGHT_PAR T_USE TT_LEFT_PAR closure_use_list TT_RIGHT_PAR TT_LEFT_BRACE stmt_list TT_RIGHT_BRACE
        { Ast.Closure (true, $4, $8, $11) }
    | T_VARIABLE TT_LEFT_PAR argument_call_list TT_RIGHT_PAR { Ast.Invoke (Ast.Assignable(Ast.Variable $1), $3) }
    
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
    | expr TT_INTEROGATION expr TT_COLON expr {Ast.TertiaryOperator ($1, $3, $5) }
    | expr TT_INTEROGATION TT_COLON expr {Ast.TertiaryOperator ($1, $1, $4) }
    
    | assignable TT_EQUAL expr { Ast.Assign ($1, $3) }
    | assignable TT_EQUAL TT_BITWISE_AND expr { Ast.AssignByRef ($1, $4) }
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
    
    | namespaced_identifier TT_LEFT_PAR argument_call_list TT_RIGHT_PAR { Ast.FunctionCall ($1, $3) }
    | T_ARRAY TT_LEFT_PAR array_content_list TT_RIGHT_PAR { Ast.ArrayConstructor $3 }
    | T_NEW namespaced_identifier TT_LEFT_PAR argument_call_list TT_RIGHT_PAR { Ast.NewObject ($2, $4) }
    | class_reference T_DOUBLE_COLON T_STRING TT_LEFT_PAR argument_call_list TT_RIGHT_PAR { Ast.StaticMethodCall ($1, $3, $5) }
    | class_reference T_DOUBLE_COLON T_STRING { Ast.ClassConstant ($1, $3) }
    
    | T_INCLUDE expr { Ast.Include ($2, false, false) }
    | T_INCLUDE_ONCE expr { Ast.Include ($2, false, true) }
    | T_REQUIRE expr { Ast.Include ($2, true, false) }
    | T_REQUIRE_ONCE expr { Ast.Include ($2, true, true) }
    
    | fluent { $1 }
    
class_reference:
    | namespaced_identifier { Ast.ClassName $1 }
    | T_SELF { Ast.Self }
    | T_STATIC { Ast.Static }
    | T_PARENT { Ast.Parent }
;
%%
