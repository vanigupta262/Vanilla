%{
open Ast  
%}

%token <int> INT
%token <bool> BOOL
%token <float> FLOATS
%token <string> VARIABLE
%token <int * string> VECTOR
%token <int * int * string> MATRIX
%token <string option> INPUT
%token <string option> PRINT
%token EOF

%token PLUS MINUS TIMES DIV MOD DOT ANGLE POW
%token ASSIGNMENT
%token EQ NEQ LESSTHAN LESSEQUAL GREATERTHAN GREATEREQUAL
%token CONJ DISJ NEG ABS DIM 
%token TRANSPOSE DET INV MINOR

%token IF ELSE FOR WHILE
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON LBRACKET RBRACKET COMMA
%token TYPE_INT TYPE_BOOL TYPE_FLOAT TYPE_VECTOR TYPE_MATRIX


%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

/* Precedence and Associativity */
%left CONJ DISJ                    /* Logical operators (&&, ||) */
%left EQ NEQ LESSTHAN LESSEQUAL GREATERTHAN GREATEREQUAL  /* Comparison operators */
%left PLUS MINUS                   /* Addition and subtraction */
%left TIMES DIV MOD               /* Multiplication, division, modulus */
%left POW                        /* Exponentiation */
%left DOT ANGLE                    /* Vector operations */
%right NEG ABS DIM TRANSPOSE DET INV  MINOR  /* Unary operators */
%right ASSIGNMENT   
%left SEMICOLON               /* Assignment operator (:=) */

%start main
%type <Ast.expr> main  

%%

/* Rest of your grammar rules remain unchanged */

main:
  
  | multistmt EOF {   $1 }
  
stmt:
  | IF LPAREN expr RPAREN LBRACE multistmt RBRACE %prec LOWER_THAN_ELSE { Ast.If($3, $6, None) }
  | TYPE_INT VARIABLE ASSIGNMENT expr SEMICOLON 
      { Ast.Seq(Ast.Declare(Ast.TInt, $2), Ast.Assign(Ast.Var($2), $4)) }
  | TYPE_FLOAT VARIABLE ASSIGNMENT expr SEMICOLON 
      { Ast.Seq(Ast.Declare(Ast.TFloat, $2), Ast.Assign(Ast.Var($2), $4)) }
  | TYPE_BOOL VARIABLE ASSIGNMENT expr SEMICOLON 
      { Ast.Seq(Ast.Declare(Ast.TBool, $2), Ast.Assign(Ast.Var($2), $4)) }
  | TYPE_VECTOR LPAREN TYPE_INT COMMA INT RPAREN VARIABLE ASSIGNMENT expr SEMICOLON 
      { Ast.Seq(Ast.Declare(Ast.TVectorInt($5), $7), Ast.Assign(Ast.Var($7), $9)) }
  | TYPE_VECTOR LPAREN TYPE_FLOAT COMMA INT RPAREN VARIABLE ASSIGNMENT  expr SEMICOLON 
      { Ast.Seq(Ast.Declare(Ast.TVectorFloat($5), $7), Ast.Assign(Ast.Var($7), $9)) }
  | TYPE_MATRIX LPAREN TYPE_INT COMMA INT COMMA INT RPAREN VARIABLE ASSIGNMENT expr SEMICOLON 
      { Ast.Seq(Ast.Declare(Ast.TMatrixInt($5, $7), $9), Ast.Assign(Ast.Var($9), $11)) }
  | TYPE_MATRIX LPAREN TYPE_FLOAT COMMA INT COMMA INT RPAREN VARIABLE ASSIGNMENT expr SEMICOLON 
      { Ast.Seq(Ast.Declare(Ast.TMatrixFloat($5, $7), $9), Ast.Assign(Ast.Var($9), $11)) }
  | IF LPAREN expr RPAREN LBRACE multistmt RBRACE ELSE LBRACE multistmt RBRACE{ Ast.If($3, $6, Some $10) }
  
 
  
  
  | PRINT SEMICOLON { Ast.Print($1) }
  | FOR LPAREN stmt    expr SEMICOLON stmt RPAREN LBRACE multistmt RBRACE
      { Ast.For($3, $4, $6, $9) }
  | WHILE LPAREN expr RPAREN LBRACE multistmt RBRACE { Ast.While($3, $6) }
  | TYPE_INT VARIABLE SEMICOLON  { Ast.Declare(Ast.TInt, $2) }
  | TYPE_BOOL VARIABLE SEMICOLON{ Ast.Declare(Ast.TBool, $2) }
  | TYPE_FLOAT VARIABLE SEMICOLON{ Ast.Declare(Ast.TFloat, $2) }
  | TYPE_VECTOR LPAREN TYPE_INT COMMA INT RPAREN VARIABLE SEMICOLON{ Ast.Declare(Ast.TVectorInt($5), $7) }
  | TYPE_VECTOR LPAREN TYPE_FLOAT COMMA INT RPAREN VARIABLE SEMICOLON{ Ast.Declare(Ast.TVectorFloat($5), $7) }

  | TYPE_MATRIX LPAREN TYPE_INT COMMA INT COMMA INT RPAREN VARIABLE SEMICOLON{ Ast.Declare(Ast.TMatrixInt($5, $7), $9) }
  | TYPE_MATRIX LPAREN TYPE_FLOAT COMMA INT COMMA INT RPAREN VARIABLE SEMICOLON { Ast.Declare(Ast.TMatrixFloat($5, $7), $9) }
  
  | expr ASSIGNMENT expr SEMICOLON { Ast.Assign($1, $3) }
  | expr SEMICOLON { $1 }
  
 
  
  

  
multistmt:
  | stmt { $1 }
  |  multistmt stmt { Ast.Seq($1, $2) }



expr:
  | INT { Ast.IntConst($1) }
  | FLOATS { Ast.FloatConst($1) }
  | BOOL { Ast.BoolConst($1) }
  | VARIABLE { Ast.Var($1) }
  | VECTOR { Ast.Vector(fst($1), snd($1)) }
  | MATRIX { let (a,b,c) = $1 in Ast.Matrix(a,b,c) }
  | expr CONJ expr { Ast.Conj($1, $3) }
    | expr DISJ expr { Ast.Disj($1, $3) }
  | INPUT  { Ast.Input($1) }
  | POW LPAREN expr COMMA expr RPAREN { Ast.Pow($3, $5) }
  
    
  
  | expr PLUS expr { Ast.Plus($1, $3) }

  | expr MINUS expr { Ast.Minus($1, $3) }
  | expr TIMES expr { Ast.Times($1, $3) }
  | expr DIV expr { Ast.Div($1, $3) }
  | expr MOD expr { Ast.Mod($1, $3) }
  | expr EQ expr { Ast.Eq($1, $3) }
  | expr NEQ expr { Ast.Neq($1, $3) }
  | expr LESSTHAN expr { Ast.LessThan($1, $3) }
  | expr LESSEQUAL expr { Ast.LessEqual($1, $3) }
  | expr GREATERTHAN expr { Ast.GreaterThan($1, $3) }
  | expr GREATEREQUAL expr { Ast.GreaterEqual($1, $3) }
  | NEG LPAREN expr RPAREN { Ast.Neg($3) }
  | ABS LPAREN expr RPAREN { Ast.Abs($3) }
  | DIM LPAREN expr RPAREN{ Ast.Dim($3) }
  | TRANSPOSE LPAREN expr RPAREN { Ast.Transpose($3) }
  | DET LPAREN expr RPAREN { Ast.Determinant($3) }
  | MINOR LPAREN VARIABLE COMMA expr COMMA expr RPAREN { Ast.Minor($3, $5, $7) }
  | INV LPAREN expr RPAREN { Ast.Inverse($3) }
  | LPAREN expr RPAREN { $2 }
  | expr DOT expr { Ast.Dot($1, $3) }
  | ANGLE LPAREN expr COMMA expr RPAREN { Ast.Angle($3, $5) }

  
  | VARIABLE LBRACKET expr RBRACKET { Ast.VectorIndex($1, $3) }
| VARIABLE LBRACKET expr COMMA expr RBRACKET { Ast.MatrixIndex($1, $3, $5) }
| MINUS expr %prec NEG { Ast.Neg($2) }

