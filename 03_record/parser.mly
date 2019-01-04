%token <int> INT
%token <string> VAR
%token COMMA
%token LBRACE
%token RBRACE
%token EQ
%token SKIP
%token LOCAL
%token IN
%token END
%token PROC
%token DOLLAR
%token EOF

%start <Ast.t> prog 

%%

prog:
  | s = statement; EOF { s }
  ;

value:
  | n = INT { Ast.Integer n }
  | PROC; LBRACE; DOLLAR; args = list(VAR); RBRACE; s = statement; END { Ast.Proc (args, s) }

statement:
  | s1 = simple_statement; COMMA; s2 = statement { Ast.Seq (s1, s2) }
  | s = simple_statement { s }
  ;

simple_statement:
  | SKIP { Ast.Skip }
  | x1 = VAR; EQ; x2 = VAR { Ast.VarBind (x1, x2) }  
  | x = VAR; EQ; v = value { Ast.ValBind (x, v) }
  | LOCAL; x = VAR; IN; s = statement ; END { Ast.Declare (x, s) }
  | LBRACE; f = VAR; args = list (VAR); RBRACE { Ast.ProcCall (f, args) }
  ;
