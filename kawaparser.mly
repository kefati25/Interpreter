%{
  open Lexing
  open Kawa
%}

%token <int> INT
%token <string> IDENT
%token <bool> BOOL

%token VOID 


%token MAIN 
%token METHOD RETURN NEW THIS
%token CLASS ATTRIBUTE COMMA EXTENDS POINT 
%token IF ELSE WHILE
%token LPAR RPAR BEGIN END SEMI 
%token EOF PRINT
%token EXCLA 
%token TRUE FALSE REM LT LE  EQ NEQ AND OR
%token ADD SUB MUL DIV OPP
%token VAR ASSIGN LB RB

/// Prioritées 

%left AND OR 
%right EQ       
%nonassoc NEQ  
%nonassoc LT LE     
%left ADD SUB     
%left MUL DIV REM 
%right POINT
%right OPP
%nonassoc LPAR RPAR

 
%start program
%type <Kawa.program> program

%%

program:
| var1 = list(var_decl) cls = list(class_def) MAIN BEGIN main=list(instruction) END EOF
 { {classes=cls; globals=var1; main} }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
| m = mem ASSIGN e = expression SEMI  {Set(m, e)}
| IF LPAR e= expression RPAR BEGIN in1 = list(instruction) END ELSE BEGIN in2 = list(instruction) END {If(e, in1, in2)}
| WHILE LPAR e = expression RPAR BEGIN instr = list(instruction) END {While(e, instr)}
| e = expression SEMI {Expr(e)} 
| RETURN e = expression SEMI {Return(e)}
;

expression:
| n=INT { Int(n) }
| TRUE { Bool(true)}
| FALSE { Bool(false) }
| THIS {This}
| LPAR e = expression RPAR { e }
| m = mem  { Get(m) }
| NEW id = IDENT {New(id)}
| NEW id = IDENT LPAR e = separated_list(COMMA, expression) RPAR {NewCstr(id, e)}
| u = uop e = expression %prec OPP{ Unop(u, e) }
| e1 = expression b = bop e2 = expression { Binop(b, e1, e2) }
| e = expression POINT id = IDENT LPAR  e1 = separated_list(COMMA, expression) RPAR {MethCall(e, id, e1)}
| NEW t = typ  e = nonempty_list(array_dim) {CreationTab(t, e)}
| BEGIN seq = separated_list(COMMA, expression) END {CreationTabInit( seq )}
;

array_dim:
| LB e = expression RB {e}
;

uop:
| SUB   {Opp}
| EXCLA {Not}
;

%inline bop:
| ADD {Add}
| SUB {Sub}
| MUL {Mul}
| DIV {Div}
| REM {Rem}
| LT {Lt}  (* Lower than *)
| LE {Le}  (* Lower or equal *)
| AND {And}
| OR {Or}
| EQ {Eq}
| NEQ {Neq}
;

var_decl:
| VAR t=typ id=IDENT ASSIGN  e= expression SEMI{(id, Some e,t)}
| VAR t=typ id=IDENT SEMI {(id,None, t)}
;



typ:
| INT {TInt}
| BOOL {TBool}
| id=IDENT {TClass(id)}
| VOID { TVoid}
| t = typ LB RB { TArray(t) }
;
mem:
| id=IDENT {Var(id)}
| e = expression POINT id = IDENT {Field(e,id)}
| m = mem LB i = expression RB {ArrayAcces(m,i)} 
;

class_def:
| CLASS id = IDENT par = option(opt_extends) BEGIN  attr = list(attr_decl) meth = list(method_def) END
{{class_name = id; attributes = attr ; methods = meth; parent = par }}
;
opt_extends:
| EXTENDS id = IDENT {(id) }
;

attr_decl:
| ATTRIBUTE t=typ id=IDENT SEMI {(id, None, t)}
| ATTRIBUTE t=typ id=IDENT EQ e= expression SEMI{(id, Some e,t)}
;


method_def:
| METHOD t=typ id=IDENT LPAR par = separated_list(COMMA, param) RPAR BEGIN var = list(var_decl) ins=list(instruction) END
{ {method_name = id; code = ins; params = par; locals = var; return = t }}
;

param:
| t=typ id=IDENT { (id, t) }
;