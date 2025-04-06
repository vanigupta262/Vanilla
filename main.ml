open Ast
open Interpreter

(* Helper to print values *)
let string_of_value = function
  | VInt i -> "VInt " ^ string_of_int i
  | VFloat f -> "VFloat " ^ string_of_float f
  | VBool b -> "VBool " ^ string_of_bool b
  | VVectorInt v -> "VVectorInt [" ^ String.concat "; " (List.map string_of_int v) ^ "]"
  | VVectorFloat v -> "VVectorFloat [" ^ String.concat "; " (List.map string_of_float v) ^ "]"
  | VMatrixInt m -> "VMatrixInt [" ^ String.concat "; " (List.map (fun row -> "[" ^ String.concat "; " (List.map string_of_int row) ^ "]") m) ^ "]"
  | VMatrixFloat m -> "VMatrixFloat [" ^ String.concat "; " (List.map (fun row -> "[" ^ String.concat "; " (List.map string_of_float row) ^ "]") m) ^ "]"
  | VUnit -> "VUnit"

(* Test function *)
let test_eval expr_str =
  try
    let lexbuf = Lexing.from_string expr_str in
    let ast = Parser.main Lexer.token lexbuf in
    let (typ, sm) = type_of ast StringMap.empty in
    let (value, env) = eval Env.empty ast in
    Printf.printf "Input: %s\n" expr_str;
    Printf.printf "AST: %s\n" (string_of_expr ast);
    Printf.printf "Type: %s\n" (string_of_alltypes typ);
    Printf.printf "Result: %s\n\n" (string_of_value value)
  with
  | Failure msg -> Printf.printf "Error: %s\n\n" msg
  | RuntimeError msg -> Printf.printf "Runtime Error: %s\n\n" msg
  | TypeError msg -> Printf.printf "Type Error: %s\n\n" msg
  | _ -> Printf.printf "Unexpected error\n\n"


(* 
let () =
  let tests = [
    "vector(int, 3) v := 3 \n [1, 2, 3]; print(v);";                 (* Vector declaration *)
    "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; print(m);";       (* Matrix declaration *)
    "int a := 5 + 3; print(a);";                                (* Arithmetic *)
    "float b := 2.0 * 3.0; print(b);";                          (* Float multiplication *)
    "vector(float, 3) v1 := 3 \n [1.0, 2.0, 3.0]; vector(float, 3) v2 := 3 \n [4.0, 5.0, 6.0]; float d := v1 . v2; print(d);"; (* Dot product *)
    "int x := 0; for (x := 0; x < 3; x := x + 1;) { print(x); } print(x);"; (* For loop *)
    "int x := 5; if (x < 3) { print(x); } else { print(0); }";  (* If-else *)
    "matrix(int, 3, 3) m := 3, 3 \n [[2, 5, 3],[1, 2, 1],[1, 3, 4]]; int det_m := det(m); print(det_m);"; (* Determinant *)
   
    "matrix (int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; matrix(int, 2, 2) n := 2, 2 \n [[5, 6], [7, 8]]; matrix(int, 2, 2) r := m + n; print(r);"; (* Matrix multiplication *)
    "matrix (int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; matrix(int, 2, 2) n := transpose(m); print(n);"; (* Matrix addition *)
    (* TC1: Integer Square Matrix Addition (2x2) *)
  "matrix(int, 2, 2) m1 := 2, 2 \n [[1, 2], [3, 4]]; matrix(int, 2, 2) m2 := 2, 2 \n [[5, 6], [7, 8]]; matrix(int, 2, 2) r := m1 + m2; print(r);";

  (* TC2: Integer Rectangular Matrix Addition (2x3) *)
  "matrix(int, 2, 3) m1 := 2, 3 \n [[1, 2, 3], [4, 5, 6]]; matrix(int, 2, 3) m2 := 2, 3 \n [[7, 8, 9], [10, 11, 12]]; matrix(int, 2, 3) r := m1 + m2; print(r);";

  (* TC3: Float Square Matrix Addition (2x2) *)
  "matrix(float, 2, 2) m1 := 2, 2 \n [[1.0, 2.0], [3.0, 4.0]]; matrix(float, 2, 2) m2 := 2, 2 \n [[5.0, 6.0], [7.0, 8.0]]; matrix(float, 2, 2) r := m1 + m2; print(r);";

  (* TC4: Float Rectangular Matrix Addition (2x3) *)
  "matrix(float, 2, 3) m1 := 2, 3 \n [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]; matrix(float, 2, 3) m2 := 2, 3 \n [[7.0, 8.0, 9.0], [10.0, 11.0, 12.0]]; matrix(float, 2, 3) r := m1 + m2; print(r);";
  (* TC1: Integer Square Matrix Transpose (2x2) *)
  "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; matrix(int, 2, 2) r := transpose(m); print(r);";

  (* TC2: Integer Rectangular Matrix Transpose (2x3 to 3x2) *)
  "matrix(int, 2, 3) m := 2, 3 \n [[1, 2, 3], [4, 5, 6]]; matrix(int, 3, 2) r := transpose(m); print(r);";

  (* TC3: Float Square Matrix Transpose (2x2) *)
  "matrix(float, 2, 2) m := 2, 2 \n [[1.0, 2.0], [3.0, 4.0]]; matrix(float, 2, 2) r := transpose(m); print(r);";

  (* TC4: Float Rectangular Matrix Transpose (2x3 to 3x2) *)
  "matrix(float, 2, 3) m := 2, 3 \n [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]; matrix(float, 3, 2) r := transpose(m); print(r);";
  (* TC1: Integer 2x2 Matrix Determinant *)
  "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; int d := det(m); print(d);";

  (* TC2: Integer 3x3 Matrix Determinant *)
  "matrix(int, 3, 3) m := 3, 3 \n [[1, 2, 3], [4, 5, 6], [7, 8, 9]]; int d := det(m); print(d);";

  (* TC3: Float 2x2 Matrix Determinant *)
  "matrix(float, 2, 2) m := 2, 2 \n [[1.0, 2.0], [3.0, 4.0]]; float d := det(m); print(d);";

  (* TC4: Float 3x3 Matrix Determinant *)
  "matrix(float, 3, 3) m := 3, 3 \n [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]]; float d := det(m); print(d);";
  "matrix(int, 2, 3) m := 2, 3 \n [[1, 2, 3], [4, 5, 6]]; int d := det(m); print(d);";  
  "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; 
  if (det(m) != 0) { 
    int d := det(m); print(d);  
  } else { 
    print(999); 
  }";
  "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; matrix(int, 2, 2) n := 2, 2 \n [[5, 6], [7, 8]]; matrix(int, 2, 2) r := m . n; print(r);"; (* Matrix multiplication *)
  (*tc for matrix mult of int rectangular matrix*)
  "matrix(int, 2, 3) m := 2, 3 \n [[1, 2, 3], [4, 5, 6]]; matrix(int, 3, 2) n := 3, 2 \n [[7, 8], [9, 10], [11, 12]]; matrix(int, 2, 2) r := m . n; print(r);";
  (*tc for matrix mult of float rectangular matrix*)
  "matrix(float, 2, 3) m := 2, 3 \n [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]; matrix(float, 3, 2) n := 3, 2 \n [[7.0, 8.0], [9.0, 10.0], [11.0, 12.0]]; matrix(float, 2, 2) r := m . n; print(r);";
    (*tc for matrix mult of int square matrix*)
    "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; matrix(int, 2, 2) n := 2, 2 \n [[5, 6], [7, 8]]; matrix(int, 2, 2) r := m . n; print(r);";
    (*tc for matrix mult of float square matrix*)
    "matrix(float, 2, 2) m := 2, 2 \n [[1.0, 2.0], [3.0, 4.0]]; matrix(float, 2, 2) n := 2, 2 \n [[5.0, 6.0], [7.0, 8.0]]; matrix(float, 2, 2) r := m . n; print(r);";
    (*tc for matrix mult of int square and rectangular matrix*)
    "matrix(int, 3,3) m :=3 ,3 \n [[1 ,1 ,1],[1 ,1 ,1],[1 ,1 ,1]]; matrix(int ,3 ,3) n :=3 ,3 \n [[1 ,1 ,1],[1 ,1 ,1],[1 ,1 ,1]]; matrix(int ,3 ,3) r :=m . n ; print(r);";
    
    (*tc for int matrix and float matrix mult*)
    "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; matrix(float, 2, 2) n := 2, 2 \n [[5.0, 6.0], [7.0, 8.0]]; matrix(float, 2, 2) r := m . n; print(r);";
   (*tc for int matrix mult with incompatible dimension like 2*3 and 2*2 *)
   "matrix(int, 2, 3) m := 2, 3 \n [[1, 2, 3], [4, 5, 6]]; matrix(int, 2, 2) n := 2, 2 \n [[7, 8], [9, 10]]; matrix(int, 2, 2) r := m . n; print(r);";
   (*tc for float matrix mult with incompatible dimension like 2*3 and 2*2 *)
     (* TC: Dot product of int vectors *)
  "vector(int, 3) v1 := 3 \n [1, 2, 3]; vector(int, 3) v2 := 3 \n [4, 5, 6]; int r := v1 . v2; print(r);";

  (* TC: Dot product of float vectors *)
  "vector(float, 3) v1 := 3 \n [1.0, 2.0, 3.0]; vector(float, 3) v2 := 3 \n [4.0, 5.0, 6.0]; float r := v1 . v2; print(r);";
    (* TC: int vector (1x3) × matrix (3x2) = vector (1x2) *)
    "vector(int, 3) v := 3 \n [1, 2, 3]; matrix(int, 3, 2) m := 3, 2 \n [[4, 5], [6, 7], [8, 9]]; vector(int, 2) r := v . m; print(r);";

    (* TC: float vector (1x3) × matrix (3x2) = vector (1x2) *)
    "vector(float, 3) v := 3 \n [1.0, 2.0, 3.0]; matrix(float, 3, 2) m := 3, 2 \n [[4.0, 5.0], [6.0, 7.0], [8.0, 9.0]]; vector(float, 2) r := v . m; print(r);";
  (* TC: int matrix (2x3) × vector (3x1) = vector (2x1) *)
  "matrix(int, 2, 3) m := 2, 3 \n [[1, 2, 3], [4, 5, 6]]; vector(int, 3) v := 3 \n [7, 8, 9]; vector(int, 2) r := m . v; print(r);";

  (* TC: float matrix (2x3) × vector (3x1) = vector (2x1) *)
  "matrix(float, 2, 3) m := 2, 3 \n [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]; vector(float, 3) v := 3 \n [7.0, 8.0, 9.0]; vector(float, 2) r := m . v; print(r);";
  "matrix(float, 2, 2) A := 2, 2 \n [[4.0, 7.0], [2.0, 6.0]]; matrix(float, 2, 2) B := inv(A); print(B);";
  (*inv of 3x3 int matrix*)
  "matrix(int, 3, 3) A := 3, 3 \n [[1, 2, 3], [0, 1, 4], [5, 6, 0]]; matrix(int, 3, 3) B := inv(A); print(B);";
  (*inv of 2x3 float matrix*)
  "matrix(float, 2, 3) A := 2, 3 \n [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]; matrix(float, 3, 2) B := inv(A); print(B);";
  "matrix(int, 2, 2) A := 2, 2 \n [[2, 1], [5, 3]]; 
 vector(int, 2) b := 2 \n [1, 2]; 
 matrix(int, 2, 2) A_inv := inv(A); 
 vector(int, 2) x := A_inv . b; 
 print(x);";
 
 "matrix(int, 2, 2) A := 2, 2 \n [[1, 2], [3, 4]]; vector(int, 2) b := 2 \n [5, 6]; vector(int, 2) C := A.b; print(C); vector(int, 2) D := b.A; print(D);"; (* Matrix multiplication with vector *)
 "matrix(int, 2, 2) A := 2, 2 \n [[2, 1], [5, 3]]; 

vector(int, 2) b := 2 
 [4, 11];

matrix(int, 2, 2) A_inv := inv(A); 

vector(int, 2) x := A_inv . b; 

print(x);
";

"vector(int, 3) v0 := 3
 [1, 2, 3];
vector(int, 3) v1 := 3 
[4, 5, 6];
vector(int, 3) v2 := 3 
[-1, -1, -1];

vector(int, 3) vector_sum := 3
 [0, 0, 0];
vector_sum := vector_sum + v0;
vector_sum := vector_sum + v1;
vector_sum := vector_sum + v2;

print(vector_sum);
";
"
vector(int, 3) vector_sum := 3 
[0, 0, 0];

vector(int, 3) V0 := 3 
[1, 2, 3];
vector(int, 3) V1 := 3 
[4, 5, 6];
vector(int, 3) V2 := 3 
[7, 8, 9];

int n := 3;

for (int i := 0; i < n; i := i + 1;) {
    if (i = 0) {
        vector_sum := vector_sum + V0;
        print(vector_sum);
    } else {
        vector_sum := vector_sum + V2;
        print(vector_sum);
    }
}

print(vector_sum);
";
"int n := 5;

vector(int, 3) vector_sum := 3 
[0, 0, 0];

matrix(int, 5, 3) V := 5, 3 
[[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12], [13, 14, 15]];

for (int i := 0; i < n; i := i + 1;) {
    vector_sum := vector_sum + V[i];
}

print(vector_sum);
";
"matrix(int, 2, 2) A := 2, 2 
[[3, 4], [0, 0]];

int sum_of_squares := 0;

for (int i := 0; i < 2; i := i + 1;) {
  for (int j := 0; j < 2; j := j + 1;) {
    sum_of_squares := sum_of_squares + A[i, j] * A[i, j];
  }
}

float magnitude_of_matrix := pow(sum_of_squares, 0.5);
print(magnitude_of_matrix);
";
"matrix(float, 2, 2) A := 2, 2 
[[0.01, 0.0],[0.0, 0.01]];

float threshold := 1e-6;
float norm_diff := det(A);

while (norm_diff > threshold) {
  A := A.A;
  norm_diff := det(A) - threshold;
}

print(A);
";
"vector(int, 3) v := 3
[1,2,3];
v[1] := 4;
print(v);" ;
"matrix(int, 2, 2) m := 2, 2
[[1, 2], [3, 4]];
int x := m[0, 1];
print(x);
m[0, 1] := 5;
print(m);";
"int x := input(); vector(int, 3) v := input(); print(x); print(v);"; (* Input test *)
"matrix(int, 2, 2) m := input(); print(m);";




 "int x := input(1.txt); print(x);
";
 "int z' := 0;
 print(z');";
 "int x;
 print(x);";
 (*write a tc to find minor of matrix*)
 "matrix(int, 3, 3) m := 3, 3
 [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
    int x:=1;
    int y := 1;
 matrix(int, 2, 2) a := minor(m, x, y);
  print(a);
 ";
 "int x:=input();
 vector(int, 2) v := 2
 [1,2];
 v[x] := 4;
 print(v);";
 "matrix(float,2,2) A := 2,2\n[[4.0, 7.0], [2.0, 6.0]]; 
vector(float,2) b := 2\n[1.0, 2.0];                     
float detA := det(A);
if (detA != 0.0) {
    matrix(float,2, 2) A_inverse := inv(A);
    vector(float,2) x := A_inverse . b;
    print(A_inverse);
    print(x);
}";


"matrix(int, 2, 2) A := 2, 2 
 [[1, 2], [3, 4]]; 

int trace := 5;
int d := det(A);
int D := trace * trace - 4 * d;

if (D >= 0) {
  float eigenvalue1 := (trace + pow(D, 0.50 )) / 2.0;
  float eigenvalue2 := (trace - pow(D, 0.50)) / 2.0;
  print(eigenvalue1);
  print(eigenvalue2);
} else {
  print(D);
}
";
"matrix(float, 2, 2) A := 2, 2 
[[4.0, 7.0], [2.0, 6.0]];
matrix(float, 2, 2) cofactor_matrix;
matrix(float, 2, 2) adjoint;
matrix(float, 2, 2) inverse;

if (det(A) != 0.0) {
  cofactor_matrix := 2, 2 
[[0.0, 0.0], [0.0, 0.0]];
for (int i := 0; i < 2; i := i + 1;) {
    for (int j := 0; j < 2; j := j + 1;) {
      
        matrix(float, 1, 1) minor_ij;
        minor_ij := minor(A,i,j);
        cofactor_matrix[i, j] := pow(-1, (i + j)) * det(minor_ij);
      
    }
  }
  
  adjoint := transpose(cofactor_matrix);
  inverse := (1.0 / det(A)) * adjoint;
  print(inverse);
} else {
  print(MatrixNotInvertible);
}";
"vector(int, 2) v := input(1.txt); vector(int, 2) b := input(2.txt); vector(int, 2) c := v + b; print(c); float an := angle(v, b); print(an);";

  ] in
  List.iter test_eval tests
 
  *)

  open Ast
open Interpreter

(* Helper to print values *)
let string_of_value = function
  | VInt i -> "VInt " ^ string_of_int i
  | VFloat f -> "VFloat " ^ string_of_float f
  | VBool b -> "VBool " ^ string_of_bool b
  | VVectorInt v -> "VVectorInt [" ^ String.concat "; " (List.map string_of_int v) ^ "]"
  | VVectorFloat v -> "VVectorFloat [" ^ String.concat "; " (List.map string_of_float v) ^ "]"
  | VMatrixInt m -> "VMatrixInt [" ^ String.concat "; " (List.map (fun row -> "[" ^ String.concat "; " (List.map string_of_int row) ^ "]") m) ^ "]"
  | VMatrixFloat m -> "VMatrixFloat [" ^ String.concat "; " (List.map (fun row -> "[" ^ String.concat "; " (List.map string_of_float row) ^ "]") m) ^ "]"
  | VUnit -> "VUnit"

(* Test function with numbering *)
let test_eval tc_num expr_str =
  Printf.printf "TC%d:\n" tc_num;  (* Print test case number *)
  try
    let lexbuf = Lexing.from_string expr_str in
    let ast = Parser.main Lexer.token lexbuf in
    let (typ, sm) = type_of ast StringMap.empty in
    let (value, env) = eval Env.empty ast in
    Printf.printf "Input: %s\n" expr_str;
    Printf.printf "AST: %s\n" (string_of_expr ast);
    Printf.printf "Type: %s\n" (string_of_alltypes typ);
    Printf.printf "Result: %s\n\n" (string_of_value value)
  with
  | Failure msg -> Printf.printf "Error: %s\n\n" msg
  | RuntimeError msg -> Printf.printf "Runtime Error: %s\n\n" msg
  | TypeError msg -> Printf.printf "Type Error: %s\n\n" msg
  | _ -> Printf.printf "Unexpected error\n\n"

let () =
  let tests = [
    "vector(int, 3) v := 3 \n [1, 2, 3]; print(v);";                 (* Vector declaration *)
    "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; print(m);";       (* Matrix declaration *)
    "int a := 5 + 3; print(a);";                                (* Arithmetic *)
    "float b := 2.0 * 3.0; print(b);";                          (* Float multiplication *)
    "vector(float, 3) v1 := 3 \n [1.0, 2.0, 3.0]; vector(float, 3) v2 := 3 \n [4.0, 5.0, 6.0]; float d := v1 . v2; print(d);"; (* Dot product *)
    "int x := 0; for (x := 0; x < 3; x := x + 1;) { print(x); } print(x);"; (* For loop *)
    "int x := 5; if (x < 3) { print(x); } else { print(0); }";  (* If-else *)
    "matrix(int, 3, 3) m := 3, 3 \n [[2, 5, 3],[1, 2, 1],[1, 3, 4]]; int det_m := det(m); print(det_m);"; (* Determinant *)
   
    "matrix (int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; matrix(int, 2, 2) n := 2, 2 \n [[5, 6], [7, 8]]; matrix(int, 2, 2) r := m . n; print(r);"; (* Matrix multiplication *)
    "matrix (int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; matrix(int, 2, 2) n := transpose(m); print(n);"; (* Matrix addition *)
    (* TC1: Integer Square Matrix Addition (2x2) *)
    "matrix(int, 2, 2) m1 := 2, 2 \n [[1, 2], [3, 4]]; matrix(int, 2, 2) m2 := 2, 2 \n [[5, 6], [7, 8]]; matrix(int, 2, 2) r := m1 + m2; print(r);";

    (* TC2: Integer Rectangular Matrix Addition (2x3) *)
    "matrix(int, 2, 3) m1 := 2, 3 \n [[1, 2, 3], [4, 5, 6]]; matrix(int, 2, 3) m2 := 2, 3 \n [[7, 8, 9], [10, 11, 12]]; matrix(int, 2, 3) r := m1 + m2; print(r);";

    (* TC3: Float Square Matrix Addition (2x2) *)
    "matrix(float, 2, 2) m1 := 2, 2 \n [[1.0, 2.0], [3.0, 4.0]]; matrix(float, 2, 2) m2 := 2, 2 \n [[5.0, 6.0], [7.0, 8.0]]; matrix(float, 2, 2) r := m1 + m2; print(r);";

    (* TC4: Float Rectangular Matrix Addition (2x3) *)
    "matrix(float, 2, 3) m1 := 2, 3 \n [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]; matrix(float, 2, 3) m2 := 2, 3 \n [[7.0, 8.0, 9.0], [10.0, 11.0, 12.0]]; matrix(float, 2, 3) r := m1 + m2; print(r);";
    (* TC1: Integer Square Matrix Transpose (2x2) *)
    "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; matrix(int, 2, 2) r := transpose(m); print(r);";

    (* TC2: Integer Rectangular Matrix Transpose (2x3 to 3x2) *)
    "matrix(int, 2, 3) m := 2, 3 \n [[1, 2, 3], [4, 5, 6]]; matrix(int, 3, 2) r := transpose(m); print(r);";

    (* TC3: Float Square Matrix Transpose (2x2) *)
    "matrix(float, 2, 2) m := 2, 2 \n [[1.0, 2.0], [3.0, 4.0]]; matrix(float, 2, 2) r := transpose(m); print(r);";

    (* TC4: Float Rectangular Matrix Transpose (2x3 to 3x2) *)
    "matrix(float, 2, 3) m := 2, 3 \n [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]; matrix(float, 3, 2) r := transpose(m); print(r);";
    (* TC1: Integer 2x2 Matrix Determinant *)
    "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; int d := det(m); print(d);";

    (* TC2: Integer 3x3 Matrix Determinant *)
    "matrix(int, 3, 3) m := 3, 3 \n [[1, 2, 3], [4, 5, 6], [7, 8, 9]]; int d := det(m); print(d);";

    (* TC3: Float 2x2 Matrix Determinant *)
    "matrix(float, 2, 2) m := 2, 2 \n [[1.0, 2.0], [3.0, 4.0]]; float d := det(m); print(d);";

    (* TC4: Float 3x3 Matrix Determinant *)
    "matrix(float, 3, 3) m := 3, 3 \n [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]]; float d := det(m); print(d);";
    "matrix(int, 2, 3) m := 2, 3 \n [[1, 2, 3], [4, 5, 6]]; int d := det(m); print(d);";  
    "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; 
    if (det(m) != 0) { 
      int d := det(m); print(d);  
    } else { 
      print(999); 
    }";
    "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; matrix(int, 2, 2) n := 2, 2 \n [[5, 6], [7, 8]]; matrix(int, 2, 2) r := m . n; print(r);"; (* Matrix multiplication *)
    (*tc for matrix mult of int rectangular matrix*)
    "matrix(int, 2, 3) m := 2, 3 \n [[1, 2, 3], [4, 5, 6]]; matrix(int, 3, 2) n := 3, 2 \n [[7, 8], [9, 10], [11, 12]]; matrix(int, 2, 2) r := m . n; print(r);";
    (*tc for matrix mult of float rectangular matrix*)
    "matrix(float, 2, 3) m := 2, 3 \n [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]; matrix(float, 3, 2) n := 3, 2 \n [[7.0, 8.0], [9.0, 10.0], [11.0, 12.0]]; matrix(float, 2, 2) r := m . n; print(r);";
    (*tc for matrix mult of int square matrix*)
    "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; matrix(int, 2, 2) n := 2, 2 \n [[5, 6], [7, 8]]; matrix(int, 2, 2) r := m . n; print(r);";
    (*tc for matrix mult of float square matrix*)
    "matrix(float, 2, 2) m := 2, 2 \n [[1.0, 2.0], [3.0, 4.0]]; matrix(float, 2, 2) n := 2, 2 \n [[5.0, 6.0], [7.0, 8.0]]; matrix(float, 2, 2) r := m . n; print(r);";
    (*tc for matrix mult of int square and rectangular matrix*)
    "matrix(int, 3,3) m :=3 ,3 \n [[1 ,1 ,1],[1 ,1 ,1],[1 ,1 ,1]]; matrix(int ,3 ,3) n :=3 ,3 \n [[1 ,1 ,1],[1 ,1 ,1],[1 ,1 ,1]]; matrix(int ,3 ,3) r :=m . n ; print(r);";
    
    (*tc for int matrix and float matrix mult*)
    "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; matrix(float, 2, 2) n := 2, 2 \n [[5.0, 6.0], [7.0, 8.0]]; matrix(float, 2, 2) r := m . n; print(r);";
    (*tc for int matrix mult with incompatible dimension like 2*3 and 2*2 *)
    "matrix(int, 2, 3) m := 2, 3 \n [[1, 2, 3], [4, 5, 6]]; matrix(int, 2, 2) n := 2, 2 \n [[7, 8], [9, 10]]; matrix(int, 2, 2) r := m . n; print(r);";
    (*tc for float matrix mult with incompatible dimension like 2*3 and 2*2 *)
    (* TC: Dot product of int vectors *)
    "vector(int, 3) v1 := 3 \n [1, 2, 3]; vector(int, 3) v2 := 3 \n [4, 5, 6]; int r := v1 . v2; print(r);";

    (* TC: Dot product of float vectors *)
    "vector(float, 3) v1 := 3 \n [1.0, 2.0, 3.0]; vector(float, 3) v2 := 3 \n [4.0, 5.0, 6.0]; float r := v1 . v2; print(r);";
    (* TC: int vector (1x3) × matrix (3x2) = vector (1x2) *)
    "vector(int, 3) v := 3 \n [1, 2, 3]; matrix(int, 3, 2) m := 3, 2 \n [[4, 5], [6, 7], [8, 9]]; vector(int, 2) r := v . m; print(r);";

    (* TC: float vector (1x3) × matrix (3x2) = vector (1x2) *)
    "vector(float, 3) v := 3 \n [1.0, 2.0, 3.0]; matrix(float, 3, 2) m := 3, 2 \n [[4.0, 5.0], [6.0, 7.0], [8.0, 9.0]]; vector(float, 2) r := v . m; print(r);";
    (* TC: int matrix (2x3) × vector (3x1) = vector (2x1) *)
    "matrix(int, 2, 3) m := 2, 3 \n [[1, 2, 3], [4, 5, 6]]; vector(int, 3) v := 3 \n [7, 8, 9]; vector(int, 2) r := m . v; print(r);";

    (* TC: float matrix (2x3) × vector (3x1) = vector (2x1) *)
    "matrix(float, 2, 3) m := 2, 3 \n [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]; vector(float, 3) v := 3 \n [7.0, 8.0, 9.0]; vector(float, 2) r := m . v; print(r);";
    "matrix(float, 2, 2) A := 2, 2 \n [[4.0, 7.0], [2.0, 6.0]]; matrix(float, 2, 2) B := inv(A); print(B);";
    (*inv of 3x3 int matrix*)
    "matrix(int, 3, 3) A := 3, 3 \n [[1, 2, 3], [0, 1, 4], [5, 6, 0]]; matrix(int, 3, 3) B := inv(A); print(B);";
    (*inv of 2x3 float matrix*)
    "matrix(float, 2, 3) A := 2, 3 \n [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]; matrix(float, 3, 2) B := inv(A); print(B);";
    "matrix(int, 2, 2) A := 2, 2 \n [[2, 1], [5, 3]]; 
    vector(int, 2) b := 2 \n [1, 2]; 
    matrix(int, 2, 2) A_inv := inv(A); 
    vector(int, 2) x := A_inv . b; 
    print(x);";
 
    "matrix(int, 2, 2) A := 2, 2 \n [[1, 2], [3, 4]]; vector(int, 2) b := 2 \n [5, 6]; vector(int, 2) C := A.b; print(C); vector(int, 2) D := b.A; print(D);"; (* Matrix multiplication with vector *)
    "matrix(int, 2, 2) A := 2, 2 \n [[2, 1], [5, 3]]; 
    vector(int, 2) b := 2 \n [4, 11];
    matrix(int, 2, 2) A_inv := inv(A); 
    vector(int, 2) x := A_inv . b; 
    print(x);";

    "vector(int, 3) v0 := 3 \n [1, 2, 3]; vector(int, 3) v1 := 3 \n [4, 5, 6]; vector(int, 3) v2 := 3 \n [-1, -1, -1]; vector(int, 3) vector_sum := 3 \n [0, 0, 0]; vector_sum := vector_sum + v0; vector_sum := vector_sum + v1; vector_sum := vector_sum + v2; print(vector_sum);";
    "vector(int, 3) vector_sum := 3 \n [0, 0, 0]; vector(int, 3) V0 := 3 \n [1, 2, 3]; vector(int, 3) V1 := 3 \n [4, 5, 6]; vector(int, 3) V2 := 3 \n [7, 8, 9]; int n := 3; for (int i := 0; i < n; i := i + 1;) { if (i = 0) { vector_sum := vector_sum + V0; print(vector_sum); } else { vector_sum := vector_sum + V2; print(vector_sum); } } print(vector_sum);";
    "int n := 5; vector(int, 3) vector_sum := 3 \n [0, 0, 0]; matrix(int, 5, 3) V := 5, 3 \n [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12], [13, 14, 15]]; for (int i := 0; i < n; i := i + 1;) { vector_sum := vector_sum + V[i]; } print(vector_sum);";
    "matrix(int, 2, 2) A := 2, 2 \n [[3, 4], [0, 0]]; int sum_of_squares := 0; for (int i := 0; i < 2; i := i + 1;) { for (int j := 0; j < 2; j := j + 1;) { sum_of_squares := sum_of_squares + A[i, j] * A[i, j]; } } float magnitude_of_matrix := pow(sum_of_squares, 0.5); print(magnitude_of_matrix);";
    "matrix(float, 2, 2) A := 2, 2 \n [[0.01, 0.0],[0.0, 0.01]]; float threshold := 1e-6; float norm_diff := det(A); while (norm_diff > threshold) { A := A.A; norm_diff := det(A) - threshold; } print(A);";
    "vector(int, 3) v := 3 \n [1,2,3]; v[1] := 4; print(v);";
    "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; int x := m[0, 1]; print(x); m[0, 1] := 5; print(m);";
    "int x := input(4.txt); vector(int, 3) v := input(5.txt); print(x); print(v);"; (* Input test *)
    "matrix(int, 2, 2) m := input(6.txt); print(m);";
    "int x := input(1.txt); print(x);";
    "int z' := 0; print(z');";
    "int x; print(x);";
    (*write a tc to find minor of matrix*)
    "matrix(int, 3, 3) m := 3, 3 \n [[1, 2, 3], [4, 5, 6], [7, 8, 9]]; int x:=1; int y := 1; matrix(int, 2, 2) a := minor(m, x, y); print(a);";
    "int x:=input(3.txt); vector(int, 2) v := 2 \n [1,2]; v[x] := 4; print(v);";
    "matrix(float,2,2) A := 2,2\n[[4.0, 7.0], [2.0, 6.0]]; vector(float,2) b := 2\n[1.0, 2.0]; float detA := det(A); if (detA != 0.0) { matrix(float,2, 2) A_inverse := inv(A); vector(float,2) x := A_inverse . b; print(A_inverse); print(x); }";
    "matrix(int, 2, 2) A := 2, 2 \n [[1, 2], [3, 4]]; int trace := 5; int d := det(A); int D := trace * trace - 4 * d; if (D >= 0) { float eigenvalue1 := (trace + pow(D, 0.50 )) / 2.0; float eigenvalue2 := (trace - pow(D, 0.50)) / 2.0; print(eigenvalue1); print(eigenvalue2); } else { print(D); }";
    "matrix(float, 2, 2) A := 2, 2 \n [[4.0, 7.0], [2.0, 6.0]]; matrix(float, 2, 2) cofactor_matrix; matrix(float, 2, 2) adjoint; matrix(float, 2, 2) inverse; if (det(A) != 0.0) { cofactor_matrix := 2, 2 \n [[0.0, 0.0], [0.0, 0.0]]; for (int i := 0; i < 2; i := i + 1;) { for (int j := 0; j < 2; j := j + 1;) { matrix(float, 1, 1) minor_ij; minor_ij := minor(A,i,j); cofactor_matrix[i, j] := pow(-1, (i + j)) * det(minor_ij); } } adjoint := transpose(cofactor_matrix); inverse := (1.0 / det(A)) * adjoint; print(inverse); } else { print(MatrixNotInvertible); }";
    "vector(int, 2) v := input(1.txt); vector(int, 2) b := input(2.txt); vector(int, 2) c := v + b; print(c); float an := angle(v, b); print(an);";

    (* New Test Cases *)
    "matrix(int, 2, 3) m := 2, 3 \n [[1, 2, 3], [4, 5, 6]]; vector(int, 3) v := 3 \n [0, 0, 0]; vector(int, 2) r := m . v; print(r);"; (* Matrix-Vector Multiplication with Zero Vector *)
    "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [2, 4]]; matrix(int, 2, 2) inv_m := inv(m); print(inv_m);"; (* Matrix Inverse of Singular Matrix *)
    "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; int sum := 0; for (int i := 0; i < 2; i := i + 1;) { for (int j := 0; j < 2; j := j + 1;) { sum := sum + m[i, j]; } } print(sum);"; (* Nested Loops with Matrix Indexing *)
    "vector(int, 3) v1 := 3 \n [1, -2, 3]; vector(int, 3) v2 := 3 \n [-4, 5, -6]; vector(int, 3) r := v1 + v2; print(r);"; (* Vector Addition with Negative Values *)
    "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; matrix(int, 2, 2) r := 3 * m; print(r);"; (* Matrix Scalar Multiplication *)
    "vector(int, 3) v := 3 \n [1, 2, 3]; matrix(int, 3, 3) m := 3, 3 \n [[1, 2, 3], [4, 5, 6], [7, 8, 9]]; vector(int, 3) r := v + m; print(r);"; (* Type Mismatch Error *)
    "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; int d := det(m); if (d > 0) { print(1); }  else { print(0); }"; (* Complex Conditional with Determinant *)
    "vector(float, 3) v1 := 3 \n [0.0001, 0.0002, 0.0003]; vector(float, 3) v2 := 3 \n [0.0004, 0.0005, 0.0006]; float r := v1 . v2; print(r);"; (* Float Vector Dot Product with Small Values *)
    "matrix(int, 2, 2) m := 2, 2 \n [[1, 2], [3, 4]]; int x := m[2, 0]; print(x);"; (* Matrix Indexing Out of Bounds *)
    "vector(int, 3) v := 3 \n [1, 2, 3]; int x := 0; for (x := 5; x < 3; x := x + 1;) { v[0] := 10; } print(v);" ;(* Empty Loop with Vector Initialization *)
    
    ] in
  (* Iterate over tests with numbering *)
  List.iteri (fun i expr -> test_eval (i + 1) expr) tests