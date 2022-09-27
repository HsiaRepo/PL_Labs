%{
   open Javascript_ast;;
%}

%token EOF           /* end-of-file */

/* Keywords */
%token LCB_KW        /* { */
%token RCB_KW        /* } */
%token LP_KW         /* ( */
%token RP_KW         /* ) */
%token LSB_KW        /* [ */
%token RSB_KW        /* ] */
%token CONSOLE_KW    /* console */
%token LOG_KW        /* log */
%token NAN_KW        /* NaN */
%token INFINITY_KW   /* Infinity */
%token CONST_KW      /* const */
%token FALSE_KW      /* false */
%token FUNC_KW       /* function */
%token LET_KW        /* let */
%token RET_KW        /* return */
%token TRUE_KW       /* true */
%token UNDEF_KW      /* undefined */

/* Operators */
%token SEMICOLON_OP  /* ; */
%token SUB_OP        /* - */
%token LT_OP         /* < */
%token GT_OP         /* > */
%token COND_OP       /* ? */
%token COLON_OP      /* : */
%token DOT_OP        /* . */
%token LOG_NOT_OP    /* ! */
%token MUL_OP        /* * */
%token DIV_OP        /* / */
%token ADD_OP        /* + */
%token COMMA_OP      /* , */
%token ASSIGN_OP     /* = */
%token LEQ_OP        /* <= */
%token GEQ_OP        /* >= */
%token STREQ_OP      /* === */
%token NSTREQ_OP     /* !== */
%token LOG_AND_OP    /* && */
%token LOG_OR_OP     /* || */

%token <float> NUMBER  /* 3.14 */

%token <string> IDENT  /* foo */
%token <string> STRING /* "bar" */

%token MUL_OPTI_LINE_COMMENT /* //* */
%token SINGLE_LINE_COMMENT   /* // */

%token BLANKS         /* [ ]\r\n\t */

/* starting with lowest precedence: */
/* DONE: fixed associativity and precedence */
%nonassoc COLON_OP
%nonassoc COND_OP
%left LOG_OR_OP
%left LOG_AND_OP
%left STREQ_OP NSTREQ_OP
%left LEQ_OP LT_OP GEQ_OP GT_OP
%left SUB_OP ADD_OP
%left DIV_OP MUL_OP
%nonassoc NEG_UOP POS_UOP
%nonassoc LOG_NOT_OP
%left DOT_OP
%nonassoc LP_KW
/*(* ^^ highest precedence / tightest binding *)*/

%start start /*(* the entry point *)*/
%type <Javascript_ast.start_t> start

%%

start:
  | program EOF {JavascriptProgram(get_current_pos (),$1)}
;


program:
  | expr {ExprProgram(get_current_pos (),$1)}
  | stmt SEMICOLON_OP program {StmtProgram(get_current_pos (),$1,$3)}
  | empty {ExprProgram(get_current_pos (),
                       ValExpr(get_current_pos (),UndefVal))}
;

stmt:
  | CONST_KW IDENT ASSIGN_OP expr { ConstStmt(get_current_pos (), $2, $4) }
  | LET_KW IDENT ASSIGN_OP expr { LetStmt(get_current_pos (), $2, $4) }
  | expr ASSIGN_OP expr {AssignStmt(get_current_pos (),$1,$3)}
;

expr:
  | IDENT { VarExpr(get_current_pos (),$1) }
  | UNDEF_KW { ValExpr(get_current_pos (),UndefVal) }
  | value { ValExpr(get_current_pos (),$1) }
  | LCB_KW block RCB_KW { BlockExpr(get_current_pos (),$2) }
  | lambda { FuncExpr(get_current_pos (), $1) }
  | LOG_NOT_OP expr { UopExpr(get_current_pos (),NotUop,$2) }
  | expr LEQ_OP expr { BopExpr(get_current_pos (),$1,LteBop,$3) }
  | expr LT_OP expr { BopExpr(get_current_pos (),$1,LtBop,$3) }
  | expr GEQ_OP expr { BopExpr(get_current_pos (),$1,GteBop,$3) }
  | expr GT_OP expr { BopExpr(get_current_pos (),$1,GtBop,$3) }
  | expr STREQ_OP expr { BopExpr(get_current_pos (),$1,EqBop,$3) }
  | expr NSTREQ_OP expr { BopExpr(get_current_pos (),$1,NeqBop,$3) }
  | expr LOG_AND_OP expr { BopExpr(get_current_pos (),$1,AndBop,$3) }
  | expr LOG_OR_OP expr { BopExpr(get_current_pos (),$1,OrBop,$3) }
  | expr COND_OP expr COLON_OP expr { IfExpr(get_current_pos (),$1,$3,$5) }
  | CONSOLE_KW DOT_OP LOG_KW LP_KW expr RP_KW { PrintExpr(get_current_pos (),$5) }
  | expr LP_KW expr_list RP_KW { CallExpr(get_current_pos (), $1,$3) }
  | expr DOT_OP IDENT { FieldExpr(get_current_pos (),$1,$3) }
  | LCB_KW field_list RCB_KW { ObjectExpr(get_current_pos (),$2) }
  | LP_KW expr RP_KW { $2 }

  /* DONE: Binary and Unary Operators */
  /* - Binary Operators: +,-,*,/ */
  | expr SUB_OP expr {BopExpr(get_current_pos (), $1, MinusBop, $3)}
  | expr ADD_OP expr {BopExpr(get_current_pos (), $1, PlusBop, $3)}
  | expr DIV_OP expr {BopExpr(get_current_pos (), $1, DivBop, $3)}
  | expr MUL_OP expr {BopExpr(get_current_pos (), $1, TimesBop, $3)}
  | SUB_OP expr %prec NEG_UOP {UopExpr(get_current_pos (), NegUop, $2)}
  
  /* - Unary Operators: +,- */
  | POS_UOP expr {UopExpr(get_current_pos (), PosUop, $2)}
  | NEG_UOP expr {UopExpr(get_current_pos (), NegUop, $2)}
;

value:
  | NUMBER { NumVal($1) }
  | NAN_KW { NumVal(nan) }
  | INFINITY_KW { NumVal(infinity) }
  | bool { BoolVal($1) }
  | STRING { StrVal($1) }
;

lambda:
  | FUNC_KW lambda_name LP_KW ident_list RP_KW LCB_KW block RCB_KW
    {
      let (name, args, body) = ($2, $4, $7) in
      (name, List.map (fun v -> (v,None)) args, body, None)
    }
;

lambda_name:
  | empty {None}
  | IDENT {Some($1)}
;

block:
  | RET_KW expr SEMICOLON_OP  {ReturnBlock(get_current_pos (),$2)}
  | stmt SEMICOLON_OP block {StmtBlock(get_current_pos (),$1,$3)}
;

field_list:
  | empty { [] }
  | field field_list_rest { $1::(List.rev $2) }
;

field_list_rest:
  | empty {[]}
  | field_list_rest COMMA_OP field {($3)::$1}
;

expr_list:
  | empty { [] }
  | expr expr_list_rest { $1::(List.rev $2) }
;

expr_list_rest:
  | empty {[]}
  | expr_list_rest COMMA_OP expr {$3::$1}
;



/* TODO: fix ident_list to allow more than one identifier */
ident_list_rest:
  | empty {[]}
  | ident_list_rest COMMA_OP IDENT {$3::$1}
;

ident_list:
  | empty { [] }
  | IDENT ident_list_rest { $1::(List.rev $2) }
;


field:
  | IDENT COLON_OP expr { ($1, $3) }
;

bool:
  | TRUE_KW { true }
  | FALSE_KW { false }
;

empty:
  | {}
;



%%
(* footer code *)
