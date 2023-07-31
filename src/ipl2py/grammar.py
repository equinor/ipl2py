from .ipl import SysDef, Type

_sysdefs = [f'"{sd.value[1:]}"' for sd in SysDef]

GRAMMAR = rf"""
start: stmt*

?stmt: simple_stmt | compound_stmt _NEWLINE?
?compound_stmt: if_stmt | while_stmt | for_stmt | proc_def | func_def
?simple_stmt: small_stmt _NEWLINE
?small_stmt: (expr_stmt | decl_stmt | halt_stmt | return_stmt)

func_def: TYPE _func_decl suite "ENDFUNCTION"
proc_def: _func_decl suite "ENDFUNCTION"

_func_decl: "FUNCTION" NAME "(" param_list ")"

?param_list: [(param ",")* param]
param: TYPE decl_type

?decl_stmt: TYPE decl_list
?decl_list: (decl_type ",")* decl_type
?decl_type: NAME          -> decl
          | NAME"[]"      -> decl_1d
          | NAME"[,]"     -> decl_2d
          | NAME"[,,]"    -> decl_3d
          | NAME "=" test -> decl_assign

for_stmt: "FOR" NAME "FROM" expr_stmt ("DOWNTO"|"TO") expr_stmt "DO" suite "ENDFOR"
while_stmt: "WHILE" test "DO" suite "ENDWHILE"
if_stmt: "IF" test "THEN" suite ["ELSE" suite] "ENDIF"
halt_stmt: "HALT"
return_stmt: "RETURN" "(" expr_stmt ")"

suite: (small_stmt | stmt)+

?test: or_test
?or_test: and_test ("OR" and_test)*
?and_test: compare ("AND" compare)*

arg_list: (argument ",")* argument
?argument: expr_stmt | expr_stmt "=" expr_stmt
subscript_list: (expr_stmt ",")~0..2 expr_stmt

?expr_stmt: molecule "=" test -> assign
     | compare
?compare: expr
     | compare "<" expr -> lt
     | compare "<=" expr -> lte
     | compare ">" expr -> gt
     | compare ">=" expr -> gte
     | compare "=" expr -> eq
     | compare "<>" expr -> noteq
?expr: term
     | expr "+" term -> add
     | expr "-" term -> sub
?term: factor
     | term "*" factor -> mult
     | term "/" factor -> div
?factor: molecule
     | "+" factor   -> uadd
     | "-" factor   -> usub
     | "NOT" factor -> unot
?molecule: NAME "(" [arg_list] ")"     -> call
     | molecule "[" subscript_list "]" -> subscript
     | molecule "." NAME               -> attribute
     | atom
?atom: BOOL | INT | FLOAT | STRING | SYSDEF | NAME
     | "(" expr_stmt ("," expr_stmt)~1..2")" -> point
     | "(" test ")"

// .1 for priority over NAME
SYSDEF.1: "@" ({"|".join([sd for sd in _sysdefs])})
BOOL.1: /(?<!\w)(TRUE|FALSE)(?!\w)/
TYPE.1: /(?<!\w)({"|".join([type.value for type in Type])})(?!\w)/x

// IPL allows ' literals of any quantity before and/or after the string...
STRING : /(?:')*("[NON_ASCII]*.*?(?<!\\)(\\\\)*?")(?:')*/
NON_ASCII: /[æøåÆØÅ]/
FLOAT: ["-"] _DECIMAL
_DECIMAL: _UINT "." _UINT | "." _UINT
COMMENT.1: CPP_COMMENT
_NEWLINE: /\r?\n[\t ]*/+
INT: ["-"] _UINT

WHITESPACE: /[ \t\f]+/
_NL: /[\r\n]+/

%ignore COMMENT
%ignore WHITESPACE
%ignore _NL
%ignore /\\[\t \f]*\r?\n/

%import common.CPP_COMMENT
%import common.INT              -> _UINT
%import common.CNAME            -> NAME
"""
