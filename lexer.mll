(*{
(* Header: OCaml code that gets copied directly into the lexer.ml *)
open Tokens  (* This makes your token definitions available here *)

exception End_of_file  (* Optional, but common to signal EOF *)
}*)
(*{
type tokens = 
  
  | INPUT of string
  | PRINT of string


  (* Literals *)
  | INT of int
  | BOOL of bool
  | FLOATS of float
  | VARIABLE of string
  | VECTOR of int*string

  | MATRIX of int*int*string

  (* Unary operators *)
  | NEG          (* -a *)
  | ABS          (* magnitude / absolute value *)
  | DIM          (* dimension of vector/matrix *)

  (* Binary logical operators *)
  | CONJ         (* AND *)
  | DISJ         (* OR *)

  (* Binary arithmetic operators *)
  | PLUS         (* + *)
  | MINUS        (* - *)
  | TIMES        (* * : includes scalar mult *)
  | DIV          (* / *)
  | MOD          (* % *)
  | DOT          (* . : dot product *)
  | ANGLE        (* < : angle between vectors *)

  (* Assignment *)
  | ASSIGNMENT   (* := *)

  (* Comparisons *)
  | EQ           (* = *)
  | NEQ          (* != *)
  | LESSTHAN     (* < *)
  | LESSEQUAL    (* <= *)
  | GREATERTHAN  (* > *)
  | GREATEREQUAL (* >= *)

  (* Control flow *)
  | FOR
  | WHILE
  | IF 
  | ELSE
  | SEMICOLON
  | LBRACE     (* { *)
  | RBRACE     (* } *)

  
  (* Parentheses *)
  | LPAREN
  | RPAREN

  (* End of line *)
  | EOL

  (* Matrix op *)
  | TRANSPOSE
  | DET

  (* brackets *)
  | LBRACKET
  | RBRACKET

  | COMMA
  
  (* types *)
  | TYPE_INT
  | TYPE_BOOL
  | TYPE_FLOAT
  | TYPE_VECTOR
  | TYPE_MATRIX

  (* functions *)
}*)
(* lexer.mll *)
(* lexer.mll *)

{
open Parser  (* Use Parser.token *)
exception End_of_file
}

let whitespace = [' ' '\t' '\r']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let alphanum = letter | digit
let floatr = 
  (['0'-'9']+ "." ['0'-'9']+ (['e''E'] ['+' '-']? ['0'-'9']+)?)
  | (['0'-'9']+ ['e''E'] ['+' '-']? ['0'-'9']+)


rule token = parse
  | [' ' '\t' '\r'] {  token lexbuf }
  | ['\n'] { token lexbuf }  (* Skip newlines, but print *)
  | floatr as lxm { FLOATS (float_of_string lxm) }
  | ("-"? digit+ as i) whitespace* "\n" whitespace* 
    ("[" whitespace* ("-"? digit+ whitespace* ("," whitespace* "-"? digit+ whitespace*)*) "]" as ss)
      {  VECTOR (int_of_string i, ss) }

| ("-"? digit+ as i) whitespace* "\n" whitespace* 
    ("[" whitespace* ("-"? floatr whitespace* ("," whitespace* "-"? floatr whitespace*)*) "]" as ss)
      { VECTOR (int_of_string i, ss) }

  (* Matrix of integers with optional negative numbers *)
| ("-"? digit+ as i) whitespace* "," whitespace* ("-"? digit+ as j) whitespace* "\n" whitespace*
    ("[[" whitespace*
      (("-"? digit+ whitespace* ("," whitespace* "-"? digit+ whitespace*)*) "]"
      (whitespace* "," whitespace* "[" "-"? digit+ whitespace* ("," whitespace* "-"? digit+ whitespace*)* "]")*)
    whitespace* "]" as ss)
    {  MATRIX (int_of_string i, int_of_string j, ss) }

(* Matrix of floats with optional negative numbers *)
| ("-"? digit+ as i) whitespace* "," whitespace* ("-"? digit+ as j) whitespace* "\n" whitespace*
    ("[[" whitespace*
      (("-"? floatr whitespace* ("," whitespace* "-"? floatr whitespace*)*) "]"
      (whitespace* "," whitespace* "[" "-"? floatr whitespace* ("," whitespace* "-"? floatr whitespace*)* "]")*)
    whitespace* "]" as ss)
    {  MATRIX (int_of_string i, int_of_string j, ss) }

  | "true" {  BOOL true }
  | "false" { BOOL false }
  | (digit*) as lxm {  INT (int_of_string lxm) }  (* Fixed integer rule *)
  | "input(" whitespace* ([^')']+ as ss) whitespace* ")" {  INPUT (Some ss) }
  | "input(" whitespace* ")" {  INPUT None }
  | "print(" whitespace* ([^'"'')']* as ss) whitespace* ")" {  PRINT (Some ss) }
  | "print(" whitespace* ")" {  PRINT None }
  | "int" {  TYPE_INT }
  | "bool" {  TYPE_BOOL }
  | "float" { TYPE_FLOAT }
  | "vector" {  TYPE_VECTOR }
  | "matrix" { TYPE_MATRIX }
  | "abs" {   ABS }
  | "dim" {  DIM }
  | "&&" {  CONJ }
  | "||" {  DISJ }
  | "not" {  NEG }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" {  TIMES }
  | "/" {  DIV }
  | "%" {  MOD }
  | "." {  DOT }
  | "angle" {  ANGLE }
  | "minor" {  MINOR }
  | "pow" {  POW }
  | ":=" {  ASSIGNMENT }
  | "=" {  EQ }
  | "!=" { NEQ }
  | "<=" {  LESSEQUAL }
  | "<" {  LESSTHAN }
  | ">=" { GREATEREQUAL }
  | ">" {  GREATERTHAN }
  | "for" {  FOR }
  | "while" {  WHILE }
  | "if" {  IF }
  | "else" {  ELSE }
  | ";" { SEMICOLON }
  | "{" {  LBRACE }
  | "}" {  RBRACE }
  | "(" { LPAREN }
  | ")" {  RPAREN }
  | "transpose" {  TRANSPOSE }
  | "det" { DET }
  | "inv" {  INV }
  | "[" {  LBRACKET }
  | "]" { RBRACKET }
  | "," {  COMMA }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' ''']* as lxm { VARIABLE lxm }
  | "//" [^'\n']* "\n" {  token lexbuf }
  | eof {  EOF }

  
  | _ { let tok = Lexing.lexeme lexbuf in Printf.printf "Unknown token: '%s'\n" tok; failwith "Unknown token" }
  


  