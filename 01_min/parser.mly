%token <int> INT
%token <string> ID
%token COMMA
%token LBRACE
%token RBRACE
%token EQ
%token SKIP
%token LOCAL
%token IN
%token END
%token EOF

%start <Ast.t> prog 

%%

prog:
  | s = statement; EOF { s }
  ;

statement:
  | s1 = simple_statement; COMMA; s2 = statement { Ast.Seq (s1, s2) }
  | s = simple_statement { s }
  ;

simple_statement:
  | SKIP { Ast.Skip }
  | x1 = ID; EQ; x2 = ID { Ast.VarBind (x1, x2) }
  | x = ID; EQ; n = INT { Ast.ValBind (x, Ast.Integer n) }
  | LOCAL; x = ID; IN; s = statement ; END { Ast.Declare (x, s) }
  | LBRACE; f = ID; args = list (ID); RBRACE { Ast.ProcCall (f, args) }
  ;
