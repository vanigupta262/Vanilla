open Ast

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VVectorInt of int list
  | VVectorFloat of float list
  | VMatrixInt of int list list
  | VMatrixFloat of float list list
  | VUnit

module Env = Map.Make(String)
type env = value Env.t

exception RuntimeError of string

(* Helper function to transpose a matrix *)

let transpose_matrix m =
  match m with
  | [] -> []
  | first_row :: _ ->
      let init_cols = List.init (List.length first_row) (fun _ -> []) in
      List.fold_right
        (fun row acc -> List.map2 (fun x xs -> x :: xs) row acc)
        m
        init_cols



let determinant_float m =
  match m with
  | [[a; b]; [c; d]] -> a *. d -. b *. c
  | [[a; b; c]; [d; e; f]; [g; h; i]] ->
      a *. (e *. i -. f *. h) -. b *. (d *. i -. f *. g) +. c *. (d *. h -. e *. g)
  | _ -> failwith "determinant_float: Only 2x2 and 3x3 supported"

  let inverse_float m =
    match m with
    | [[a; b]; [c; d]] ->
        let det = a *. d -. b *. c in
        if det = 0.0 then failwith "Matrix not invertible"
        else
          let inv_det = 1.0 /. det in
          [[ d *. inv_det; -.b *. inv_det];
           [-.c *. inv_det;  a *. inv_det]]
    | [[a; b; c]; [d; e; f]; [g; h; i]] ->
        let det = a *. (e *. i -. f *. h) -. b *. (d *. i -. f *. g) +. c *. (d *. h -. e *. g) in
        if det = 0.0 then failwith "Matrix not invertible"
        else
          let inv_det = 1.0 /. det in
          let cofactor_matrix = [
            [ (e *. i -. f *. h); -. (b *. i -. c *. h); (b *. f -. c *. e) ];
            [-.(d *. i -. f *. g); (a *. i -. c *. g); -. (a *. f -. c *. d) ];
            [ (d *. h -. e *. g); -. (a *. h -. b *. g); (a *. e -. b *. d) ]
          ] in
          (* Transpose the cofactor matrix and multiply by inverse of det *)
          List.map (fun row -> List.map (( *. ) inv_det) row) (transpose_matrix cofactor_matrix)
    | _ -> failwith "inverse_float: Only 2x2 and 3x3 matrices supported"
  
        
let preprocess_input input =
  let trimmed = String.trim input in
  (* Example: If input is a single number, wrap it in a valid expression *)
  try
    let _ = int_of_string trimmed in
    trimmed ^ " ;" (* If it’s just a number, return it as-is; parser should handle it *)
  with _ ->
    try
      let _ = float_of_string trimmed in
      trimmed ^ " ;" (* If it’s a float, return it *)
    with _ ->
      (* Example: If it’s a malformed vector like "1,2,3", wrap it in brackets *)
      if String.contains trimmed ',' && not (String.contains trimmed '[') then
        "[" ^ trimmed ^ "]" ^ " ;"
      else
        (* Default: return input unchanged if no specific rule applies *)
        trimmed ^ " ;"

(* Helper function to compute dot product of two vectors *)
let dot_product_int v1 v2 =
  List.fold_left2 (fun acc x y -> acc + (x * y)) 0 v1 v2

let dot_product_float v1 v2 =
  List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0.0 v1 v2

(* Helper function for matrix multiplication *)
let matrix_multiply_int m1 m2 =
  let m2_t = transpose_matrix m2 in
  List.map (fun row -> List.map (dot_product_int row) m2_t) m1

let matrix_multiply_float m1 m2 =
  let m2_t = transpose_matrix m2 in
  List.map (fun row -> List.map (dot_product_float row) m2_t) m1

(* Helper function to compute determinant (for 2x2 matrices only for simplicity) *)
let rec minor matrix row col =
  matrix
  |> List.mapi (fun i r ->
         if i = row then None
         else
           Some (
             List.mapi (fun j x -> if j = col then None else Some x) r
             |> List.filter_map Fun.id
           ))
  |> List.filter_map Fun.id

let rec determinant_int m =
  match m with
  | [] -> raise (Failure "Empty matrix has no determinant")
  | [[x]] -> x
  | [[a; b]; [c; d]] -> a * d - b * c
  | _ ->
      List.mapi (fun j elem ->
        let sign = if j mod 2 = 0 then 1 else -1 in
        let cofactor = minor m 0 j in
        sign * elem * determinant_int cofactor
      ) (List.hd m)
      |> List.fold_left ( + ) 0

      let rec minor_float matrix row col =
        matrix
        |> List.mapi (fun i r ->
               if i = row then None
               else
                 Some (
                   List.mapi (fun j x -> if j = col then None else Some x) r
                   |> List.filter_map Fun.id
                 ))
        |> List.filter_map Fun.id
      
      let rec determinant_float m =
        match m with
        | [] -> raise (Failure "Empty matrix has no determinant")
        | [[x]] -> x
        | [[a; b]; [c; d]] -> a *. d -. b *. c
        | _ ->
            List.mapi (fun j elem ->
              let sign = if j mod 2 = 0 then 1.0 else -1.0 in
              let cofactor = minor_float m 0 j in
              sign *. elem *. determinant_float cofactor
            ) (List.hd m)
            |> List.fold_left ( +. ) 0.0

            let inverse_int m =
              let float_matrix = List.map (List.map float_of_int) m in
              let inv_float =
                match float_matrix with
                | [[a; b]; [c; d]] ->
                    let det = a *. d -. b *. c in
                    if det = 0.0 then failwith "Matrix not invertible"
                    else
                      let inv_det = 1.0 /. det in
                      [[ d *. inv_det; -.b *. inv_det];
                       [-.c *. inv_det;  a *. inv_det]]
                | [[a; b; c]; [d; e; f]; [g; h; i]] ->
                    let det = a *. (e *. i -. f *. h) -. b *. (d *. i -. f *. g) +. c *. (d *. h -. e *. g) in
                    if det = 0.0 then failwith "Matrix not invertible"
                    else
                      let inv_det = 1.0 /. det in
                      let cofactor_matrix = [
                        [ (e *. i -. f *. h); -. (b *. i -. c *. h); (b *. f -. c *. e) ];
                        [-.(d *. i -. f *. g); (a *. i -. c *. g); -. (a *. f -. c *. d) ];
                        [ (d *. h -. e *. g); -. (a *. h -. b *. g); (a *. e -. b *. d) ]
                      ] in
                      let transposed = transpose_matrix cofactor_matrix in
                      List.map (List.map (fun x -> x *. inv_det)) transposed
                | _ -> failwith "inverse_int: Only 2x2 and 3x3 matrices supported"
              in
              List.map (List.map int_of_float) inv_float
            
(* Helper function to parse vector/matrix literals *)
let parse_vector_int s =
  let s = String.trim s in
  let inner = String.sub s 1 (String.length s - 2) in
  let elements = String.split_on_char ',' inner |> List.map String.trim |> List.map int_of_string in
  elements

let parse_vector_float s =
  let s = String.trim s in
  let inner = String.sub s 1 (String.length s - 2) in
  let elements = String.split_on_char ',' inner |> List.map String.trim |> List.map float_of_string in
  elements
  let parse_matrix_int s =
    let s = String.trim s in
    if String.length s < 4 then raise (Failure "Invalid matrix literal");
    let inner = String.sub s 1 (String.length s - 2) in (* remove outer [] *)
  
    (* Split into rows by locating sub-vectors *)
    let rec extract_rows acc i =
      try
        let start_idx = String.index_from inner i '[' in
        let end_idx = String.index_from inner start_idx ']' in
        let row_str = String.sub inner (start_idx + 1) (end_idx - start_idx - 1) in
        extract_rows (acc @ [row_str]) (end_idx + 1)
      with Not_found -> acc
    in
    let row_strings = extract_rows [] 0 in
    List.map parse_vector_int (List.map (fun r -> "[" ^ r ^ "]") row_strings)
  
  let parse_matrix_float s =
    let s = String.trim s in
    if String.length s < 4 then raise (Failure "Invalid matrix literal");
    let inner = String.sub s 1 (String.length s - 2) in (* remove outer [] *)
  
    (* Split into rows by locating sub-vectors *)
    let rec extract_rows acc i =
      try
        let start_idx = String.index_from inner i '[' in
        let end_idx = String.index_from inner start_idx ']' in
        let row_str = String.sub inner (start_idx + 1) (end_idx - start_idx - 1) in
        extract_rows (acc @ [row_str]) (end_idx + 1)
      with Not_found -> acc
    in
    let row_strings = extract_rows [] 0 in
    List.map parse_vector_float (List.map (fun r -> "[" ^ r ^ "]") row_strings)
  
    let multiply_matrix_vector_int (m : int list list) (v : int list) : int list =
      List.map (fun row ->
        List.fold_left2 (fun acc a b -> acc + (a * b)) 0 row v
      ) m
      let multiply_vector_matrix_int (v : int list) (m : int list list) : int list =
        let cols = List.length (List.hd m) in
        List.init cols (fun j ->
          List.fold_left2 (fun acc vi mi -> acc + (vi * List.nth mi j)) 0 v m
        )

        let multiply_matrix_vector_float (m : float list list) (v : float list) : float list =
          List.map (fun row ->
            List.fold_left2 (fun acc a b -> acc +. (a *. b)) 0.0 row v
          ) m

          let multiply_vector_matrix_float (v : float list) (m : float list list) : float list =
            let cols = List.length (List.hd m) in
            List.init cols (fun j ->
              List.fold_left2 (fun acc vi mi -> acc +. (vi *. List.nth mi j)) 0.0 v m
            )
            let preprocess_file_input filename =
              let chan = open_in filename in
              let rec read_all acc =
                try
                  let line = input_line chan in
                  read_all (acc ^ line ^ "\n")
                with End_of_file -> acc
              in
              let content = read_all "" in
              close_in chan;
              preprocess_input content
            
(* Main evaluation function *)
let rec eval (env : env) (expr : expr) : (value * env) =
  match expr with
  | IntConst i -> VInt i, env
  | FloatConst f -> VFloat f, env
  | BoolConst b -> VBool b, env
  | Var x ->
      (try Env.find x env, env
       with Not_found -> raise (RuntimeError ("Variable not found: " ^ x)))
  | Vector (n, s) ->
      (match classify_vector_type s with
       | "int" -> VVectorInt (parse_vector_int s), env
       | "float" -> VVectorFloat (parse_vector_float s), env
       | _ -> raise (RuntimeError ("Invalid vector literal: " ^ s)))
  | Matrix (m, n, s) ->
      (match classify_vector_type s with
       | "int" -> VMatrixInt (parse_matrix_int s), env
       | "float" -> VMatrixFloat (parse_matrix_float s), env
       | _ -> raise (RuntimeError ("Invalid matrix literal: " ^ s)))

  | Plus (e1, e2) ->
      let v1, env1 = eval env e1 in
      let v2, env2 = eval env1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> VInt (i1 + i2), env2
       | VFloat f1, VFloat f2 -> VFloat (f1 +. f2), env2
       | VFloat f1 , VInt i2 -> VFloat (f1 +. float_of_int i2), env2
        | VInt i1, VFloat f2 -> VFloat (float_of_int i1 +. f2), env2

       | VVectorInt l1, VVectorInt l2 -> VVectorInt (List.map2 (+) l1 l2), env2
       | VVectorFloat l1, VVectorFloat l2 -> VVectorFloat (List.map2 (+.) l1 l2), env2
       | VMatrixInt m1, VMatrixInt m2 -> VMatrixInt (List.map2 (List.map2 (+)) m1 m2), env2
       | VMatrixFloat m1, VMatrixFloat m2 -> VMatrixFloat (List.map2 (List.map2 (+.)) m1 m2), env2
       | _ -> raise (RuntimeError ("Type error in addition: " ^ string_of_expr expr)))

  | Minus (e1, e2) ->
      let v1, env1 = eval env e1 in
      let v2, env2 = eval env1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> VInt (i1 - i2), env2
       | VFloat f1, VFloat f2 -> VFloat (f1 -. f2), env2
       | VFloat f1 , VInt i2 -> VFloat (f1 -. float_of_int i2), env2
        | VInt i1, VFloat f2 -> VFloat (float_of_int i1 -. f2), env2

       | VVectorInt l1, VVectorInt l2 -> VVectorInt (List.map2 (-) l1 l2), env2
       | VVectorFloat l1, VVectorFloat l2 -> VVectorFloat (List.map2 (-.) l1 l2), env2
       | VMatrixInt m1, VMatrixInt m2 -> VMatrixInt (List.map2 (List.map2 (-)) m1 m2), env2
       | VMatrixFloat m1, VMatrixFloat m2 -> VMatrixFloat (List.map2 (List.map2 (-.)) m1 m2), env2
       | _ -> raise (RuntimeError ("Type error in subtraction: " ^ string_of_expr expr)))

  | Times (e1, e2) ->
      let v1, env1 = eval env e1 in
      let v2, env2 = eval env1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> VInt (i1 * i2), env2
       | VFloat f1, VFloat f2 -> VFloat (f1 *. f2), env2
       | VFloat f1 , VInt i2 -> VFloat (f1 *. float_of_int i2), env2
        | VInt i1, VFloat f2 -> VFloat (float_of_int i1 *. f2), env2
       | VInt i, VVectorInt l | VVectorInt l, VInt i -> VVectorInt (List.map (( * ) i) l), env2
       | VFloat f, VVectorFloat l | VVectorFloat l, VFloat f -> VVectorFloat (List.map (( *. ) f) l), env2
       | VInt i, VMatrixInt m | VMatrixInt m, VInt i -> VMatrixInt (List.map (List.map (( * ) i)) m), env2
       | VFloat f, VMatrixFloat m | VMatrixFloat m, VFloat f -> VMatrixFloat (List.map (List.map (( *. ) f)) m), env2
       | _ -> raise (RuntimeError ("Type error in multiplication: " ^ string_of_expr expr)))

  | Div (e1, e2) ->
      let v1, env1 = eval env e1 in
      let v2, env2 = eval env1 e2 in
      (match v2 with
       | VInt 0 | VFloat 0.0 -> raise (RuntimeError ("Division by zero: " ^ string_of_expr expr))
       | _ ->
           match v1, v2 with
           | VInt i1, VInt i2 -> VInt (i1 / i2), env2
           | VFloat f1, VFloat f2 -> VFloat (f1 /. f2), env2
           | _ -> raise (RuntimeError ("Type error in division: " ^ string_of_expr expr)))

  | Mod (e1, e2) ->
      let v1, env1 = eval env e1 in
      let v2, env2 = eval env1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> VInt (i1 mod i2), env2
       | _ -> raise (RuntimeError ("Type error in modulo: " ^ string_of_expr expr)))

  | Neg e ->
      let v, env1 = eval env e in
      (match v with
       | VInt i -> VInt (-i), env1
       | VFloat f -> VFloat (-.f), env1
       | _ -> raise (RuntimeError ("Type error in negation: " ^ string_of_expr expr)))

  | Eq (e1, e2) ->
      let v1, env1 = eval env e1 in
      let v2, env2 = eval env1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> VBool (i1 = i2), env2
       | VFloat f1, VFloat f2 -> VBool (f1 = f2), env2
       | _ -> raise (RuntimeError ("Type error in equality: " ^ string_of_expr expr)))

  | Neq (e1, e2) ->
      let v1, env1 = eval env e1 in
      let v2, env2 = eval env1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> VBool (i1 <> i2), env2
       | VFloat f1, VFloat f2 -> VBool (f1 <> f2), env2
       | _ -> raise (RuntimeError ("Type error in inequality: " ^ string_of_expr expr)))

  | LessThan (e1, e2) ->
      let v1, env1 = eval env e1 in
      let v2, env2 = eval env1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> VBool (i1 < i2), env2
       | VFloat f1, VFloat f2 -> VBool (f1 < f2), env2
       | _ -> raise (RuntimeError ("Type error in less than: " ^ string_of_expr expr)))

  | LessEqual (e1, e2) ->
      let v1, env1 = eval env e1 in
      let v2, env2 = eval env1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> VBool (i1 <= i2), env2
       | VFloat f1, VFloat f2 -> VBool (f1 <= f2), env2
       | _ -> raise (RuntimeError ("Type error in less equal: " ^ string_of_expr expr)))

  | GreaterThan (e1, e2) ->
      let v1, env1 = eval env e1 in
      let v2, env2 = eval env1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> VBool (i1 > i2), env2
       | VFloat f1, VFloat f2 -> VBool (f1 > f2), env2
       | _ -> raise (RuntimeError ("Type error in greater than: " ^ string_of_expr expr)))

  | GreaterEqual (e1, e2) ->
      let v1, env1 = eval env e1 in
      let v2, env2 = eval env1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> VBool (i1 >= i2), env2
       | VFloat f1, VFloat f2 -> VBool (f1 >= f2), env2
       | _ -> raise (RuntimeError ("Type error in greater equal: " ^ string_of_expr expr)))

  | Conj (e1, e2) ->
      let v1, env1 = eval env e1 in
      let v2, env2 = eval env1 e2 in
      (match v1, v2 with
       | VBool b1, VBool b2 -> VBool (b1 && b2), env2
       | _ -> raise (RuntimeError ("Type error in conjunction: " ^ string_of_expr expr)))

  | Disj (e1, e2) ->
      let v1, env1 = eval env e1 in
      let v2, env2 = eval env1 e2 in
      (match v1, v2 with
       | VBool b1, VBool b2 -> VBool (b1 || b2), env2
       | _ -> raise (RuntimeError ("Type error in disjunction: " ^ string_of_expr expr)))

  | Not e ->
      let v, env1 = eval env e in
      (match v with
       | VBool b -> VBool (not b), env1
       | _ -> raise (RuntimeError ("Type error in negation: " ^ string_of_expr expr)))

  | Dot (e1, e2) ->
        let v1, env1 = eval env e1 in
        let v2, env2 = eval env1 e2 in
        (match v1, v2 with
         | VVectorInt v1, VVectorInt v2 ->
             VInt (dot_product_int v1 v2), env2
    
         | VVectorFloat v1, VVectorFloat v2 ->
             VFloat (dot_product_float v1 v2), env2
    
         | VMatrixInt m1, VMatrixInt m2 ->
             VMatrixInt (matrix_multiply_int m1 m2), env2
    
         | VMatrixFloat m1, VMatrixFloat m2 ->
             VMatrixFloat (matrix_multiply_float m1 m2), env2
    
         | VVectorInt v, VMatrixInt m ->
             if List.length v = List.length m then
               VVectorInt (multiply_vector_matrix_int v m), env2
             else raise (RuntimeError "Shape mismatch in vector × matrix")
    
         | VMatrixInt m, VVectorInt v ->
             if (List.length (List.hd m)) = List.length v then
               VVectorInt (multiply_matrix_vector_int m v), env2
             else raise (RuntimeError "Shape mismatch in matrix × vector")
    
         | VVectorFloat v, VMatrixFloat m ->
             if List.length v = List.length m then
               VVectorFloat (multiply_vector_matrix_float v m), env2
             else raise (RuntimeError "Shape mismatch in vector × matrix")
    
         | VMatrixFloat m, VVectorFloat v ->
             if (List.length (List.hd m)) = List.length v then
               VVectorFloat (multiply_matrix_vector_float m v), env2
             else raise (RuntimeError "Shape mismatch in matrix × vector")
    
         | _ ->
             raise (RuntimeError ("Type error in dot product: " ^ string_of_expr expr))
        )
    
  | Angle (e1, e2) ->
      let v1, env1 = eval env e1 in
      let v2, env2 = eval env1 e2 in
      (match v1, v2 with
       | VVectorInt v1, VVectorInt v2 ->
           let dot = float_of_int (dot_product_int v1 v2) in
           let mag1 = sqrt (float_of_int (dot_product_int v1 v1)) in
           let mag2 = sqrt (float_of_int (dot_product_int v2 v2)) in
           VFloat (acos (dot /. (mag1 *. mag2))), env2
       | VVectorFloat v1, VVectorFloat v2 ->
           let dot = dot_product_float v1 v2 in
           let mag1 = sqrt (dot_product_float v1 v1) in
           let mag2 = sqrt (dot_product_float v2 v2) in
           VFloat (acos (dot /. (mag1 *. mag2))), env2
       | _ -> raise (RuntimeError ("Type error in angle: " ^ string_of_expr expr)))

  | Pow(e1, e2) ->
      let v1, env1 = eval env e1 in
      let v2, env2 = eval env1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> VInt (int_of_float ((float_of_int i1) ** (float_of_int i2))), env2
       | VFloat f1, VFloat f2 -> VFloat (f1 ** f2), env2
       | VInt i1, VFloat f2 -> VFloat ((float_of_int i1) ** f2), env2
        | VFloat f1, VInt i2 -> VFloat (f1 ** (float_of_int i2)), env2
       | _ -> raise (RuntimeError ("Type error in power: " ^ string_of_expr expr)))

  | Transpose e ->
      let v, env1 = eval env e in
      (match v with
       | VMatrixInt m -> VMatrixInt (transpose_matrix m), env1
       | VMatrixFloat m -> VMatrixFloat (transpose_matrix m), env1
       | _ -> raise (RuntimeError ("Type error in transpose: " ^ string_of_expr expr)))

  | Determinant e ->
      let v, env1 = eval env e in
      (match v with
       | VMatrixInt m -> VInt (determinant_int m), env1
       | VMatrixFloat m -> VFloat (determinant_float m), env1
       | _ -> raise (RuntimeError ("Type error in determinant: " ^ string_of_expr expr)))

  | Abs e ->
      let v, env1 = eval env e in
      (match v with
       | VInt i -> VInt (abs i), env1
       | VFloat f -> VFloat (abs_float f), env1
       | _ -> raise (RuntimeError ("Type error in abs: " ^ string_of_expr expr)))

  | Dim e ->
      let v, env1 = eval env e in
      (match v with
       | VVectorInt v -> VInt (List.length v), env1
       | VVectorFloat v -> VInt (List.length v), env1
       | VMatrixInt m -> VInt (List.length m), env1  (* Returns number of rows *)
       | VMatrixFloat m -> VInt (List.length m), env1
       | _ -> raise (RuntimeError ("Type error in dim: " ^ string_of_expr expr)))

  | Declare (t, x) ->
      let default_value = match t with
        | TInt -> VInt 0
        | TFloat -> VFloat 0.0
        | TBool -> VBool false
        | TVectorInt n -> VVectorInt (List.init n (fun _ -> 0))
        | TVectorFloat n -> VVectorFloat (List.init n (fun _ -> 0.0))
        | TMatrixInt (m, n) -> VMatrixInt (List.init m (fun _ -> List.init n (fun _ -> 0)))
        | TMatrixFloat (m, n) -> VMatrixFloat (List.init m (fun _ -> List.init n (fun _ -> 0.0)))
      in
      VUnit, Env.add x default_value env

  | Assign (e1, e2) ->
      let v2, env1 = eval env e2 in
      (match e1 with
       | Var x -> VUnit, Env.add x v2 env1
       | VectorIndex (v, idx) ->
           let v_idx, env2 = eval env1 idx in
           (match v_idx with
            | VInt i ->
                let vec = Env.find v env2 in
                (match vec with
                 | VVectorInt lst ->
                     let new_lst = List.mapi (fun j x -> if j = i then match v2 with VInt n -> n | _ -> raise (RuntimeError "Type mismatch") else x) lst in
                     VUnit, Env.add v (VVectorInt new_lst) env2
                 | VVectorFloat lst ->
                     let new_lst = List.mapi (fun j x -> if j = i then match v2 with VFloat f -> f | _ -> raise (RuntimeError "Type mismatch") else x) lst in
                     VUnit, Env.add v (VVectorFloat new_lst) env2
                
                 | _ -> raise (RuntimeError ("Invalid vector index target: " ^ string_of_expr expr)))
            | _ -> raise (RuntimeError ("Index must be an integer: " ^ string_of_expr expr)))
       | MatrixIndex (m, row, col) ->
           let v_row, env2 = eval env1 row in
           let v_col, env3 = eval env2 col in
           (match v_row, v_col with
            | VInt r, VInt c ->
                let mat = Env.find m env3 in
                (match mat with
                 | VMatrixInt lst ->
                     let new_lst = List.mapi (fun i row_lst ->
                       if i = r then List.mapi (fun j x -> if j = c then match v2 with VInt n -> n | _ -> raise (RuntimeError "Type mismatch") else x) row_lst
                       else row_lst) lst in
                     VUnit, Env.add m (VMatrixInt new_lst) env3
                 | VMatrixFloat lst ->
                     let new_lst = List.mapi (fun i row_lst ->
                       if i = r then List.mapi (fun j x -> if j = c then match v2 with VFloat f -> f | _ -> raise (RuntimeError "Type mismatch") else x) row_lst
                       else row_lst) lst in
                     VUnit, Env.add m (VMatrixFloat new_lst) env3
                 | _ -> raise (RuntimeError ("Invalid matrix index target: " ^ string_of_expr expr)))
            | _ -> raise (RuntimeError ("Matrix indices must be integers: " ^ string_of_expr expr)))
       | _ -> raise (RuntimeError ("Invalid assignment target: " ^ string_of_expr e1)))

  | If (e1, e2, e3_opt) ->
      let v1, env1 = eval env e1 in
      (match v1 with
       | VBool true -> eval env1 e2
       | VBool false ->
           (match e3_opt with
            | Some e3 -> eval env1 e3
            | None -> VUnit, env1)
       | _ -> raise (RuntimeError ("Type error in if condition: " ^ string_of_expr expr)))

  | For (e1, e2, e3, e4) ->
      let _, env1 = eval env e1 in  (* Initialization *)
      let rec loop env =
        let v2, env2 = eval env e2 in  (* Condition *)
        match v2 with
        | VBool true ->
            let _, env3 = eval env2 e4 in  (* Body *)
            let _, env4 = eval env3 e3 in  (* Increment *)
            loop env4
        | VBool false -> VUnit, env
        | _ -> raise (RuntimeError ("Type error in for condition: " ^ string_of_expr expr))
      in
      loop env1

  | While (e1, e2) ->
      let rec loop env =
        let v1, env1 = eval env e1 in
        match v1 with
        | VBool true ->
            let _, env2 = eval env1 e2 in
            loop env2
        | VBool false -> VUnit, env
        | _ -> raise (RuntimeError ("Type error in while condition: " ^ string_of_expr expr))
      in
      loop env
    | Input s_opt ->
      let prompt = match s_opt with Some s -> s | None -> "" in
      if Filename.check_suffix prompt ".txt" || Sys.file_exists prompt then
        (* File-based input *)
        let processed_input = preprocess_file_input prompt in
        try
          let lexbuf = Lexing.from_string processed_input in
          let ast = Parser.main Lexer.token lexbuf in
          eval env ast
        with
        | Parsing.Parse_error -> raise (RuntimeError ("Invalid input syntax in file: " ^ prompt))
        | Failure msg -> raise (RuntimeError ("Failed to parse file input: " ^ msg))
        | _ -> raise (RuntimeError ("Error processing file input: " ^ prompt))
      else begin
        (* Interactive input *)
        print_string prompt;
        let rec read_lines acc =
          let line = read_line () in
          if String.trim line = "" then List.rev acc
          else read_lines (line :: acc)
        in
        let lines = read_lines [] in
        if lines = [] then (VUnit, env)
        else
          let input = String.concat "\n" lines in
          let processed_input = preprocess_input input in
          try
            let lexbuf = Lexing.from_string processed_input in
            let ast = Parser.main Lexer.token lexbuf in
            eval env ast
          with
          | Parsing.Parse_error -> raise (RuntimeError ("Invalid input syntax: " ^ input))
          | Failure msg -> raise (RuntimeError ("Failed to parse input: " ^ msg))
          | _ -> raise (RuntimeError ("Error processing input: " ^ input))
      end
    
  | Print s_opt ->
      (match s_opt with
       | Some s ->
           (match Env.find_opt s env with
            | Some (VInt i) -> print_endline (string_of_int i)
            | Some (VFloat f) -> print_endline (string_of_float f)
            | Some (VBool b) -> print_endline (string_of_bool b)
            | Some (VVectorInt v) -> print_endline (String.concat ", " (List.map string_of_int v))
            | Some (VVectorFloat v) -> print_endline (String.concat ", " (List.map string_of_float v))
            | Some (VMatrixInt m) -> List.iter (fun row -> print_endline (String.concat ", " (List.map string_of_int row))) m
            | Some (VMatrixFloat m) -> List.iter (fun row -> print_endline (String.concat ", " (List.map string_of_float row))) m
            | _ -> print_endline s)
       | None -> print_endline "");
      VUnit, env

  | Seq (e1, e2) ->
      let _, env1 = eval env e1 in
      eval env1 e2
  | Inverse e1 ->
    let (v, env1) = eval env e1 in
    (match v with
      | VMatrixInt m ->
          if determinant_int m = 0 then
            raise (RuntimeError "Matrix is not invertible (det = 0)")
          else
            let inv = inverse_int m in
            (VMatrixInt inv, env1)

      | VMatrixFloat m ->
          if determinant_float m = 0.0 then
            raise (RuntimeError "Matrix is not invertible (det = 0)")
          else
            let inv = inverse_float m in
            (VMatrixFloat inv, env1)

      | _ -> raise (RuntimeError "inverse() is only defined for matrices"))
    
      

      | VectorIndex (v, e) ->
        (let v_idx, env1 = eval env e in
        let vec = Env.find v env1 in
        match v_idx with
        | VInt i ->
            (match vec with
             | VVectorInt lst ->
                 if i < 0 || i >= List.length lst then
                   raise (RuntimeError ("Index out of bounds in vector: " ^ v))
                 else VInt (List.nth lst i), env1
             | VVectorFloat lst ->
                 if i < 0 || i >= List.length lst then
                   raise (RuntimeError ("Index out of bounds in vector: " ^ v))
                 else VFloat (List.nth lst i), env1
             | VMatrixInt lst ->
                 if i < 0 || i >= List.length lst then
                   raise (RuntimeError ("Row index out of bounds for matrix: " ^ v))
                 else VVectorInt (List.nth lst i), env1
             | VMatrixFloat lst ->
                 if i < 0 || i >= List.length lst then
                   raise (RuntimeError ("Row index out of bounds for matrix: " ^ v))
                 else VVectorFloat (List.nth lst i), env1
             | _ -> raise (RuntimeError ("Invalid target for indexing: " ^ v)))
        | _ -> raise (RuntimeError ("Index must be an integer in expression: " ^ string_of_expr expr)))
    | MatrixIndex (m, e1, e2) ->
      let v_row, env1 = eval env e1 in
      let v_col, env2 = eval env1 e2 in
      let mat = Env.find m env2 in
      (match v_row, v_col with
        | VInt r, VInt c ->
            (match mat with
            | VMatrixInt lst ->
                if r < 0 || r >= List.length lst || c < 0 || c >= List.length (List.nth lst r) then
                  raise (RuntimeError ("Index out of bounds in matrix: " ^ m))
                else VInt (List.nth (List.nth lst r) c), env2
            | VMatrixFloat lst ->
                if r < 0 || r >= List.length lst || c < 0 || c >= List.length (List.nth lst r) then
                  raise (RuntimeError ("Index out of bounds in matrix: " ^ m))
                else VFloat (List.nth (List.nth lst r) c), env2
            | _ -> raise (RuntimeError ("Invalid matrix index target: " ^ m)))
        | _ -> raise (RuntimeError ("Matrix indices must be integers in: " ^ string_of_expr expr)))
        | Minor (m, e1, e2) ->
          let v_row, env1 = eval env e1 in
          let v_col, env2 = eval env1 e2 in
          let mat = Env.find m env2 in
          (match v_row, v_col with
           | VInt r, VInt c ->
               (match mat with
                | VMatrixInt lst ->
                    if r < 0 || r >= List.length lst || c < 0 || c >= List.length (List.nth lst r) then
                      raise (RuntimeError ("Index out of bounds in matrix: " ^ m))
                    else
                      VMatrixInt (minor lst r c), env2  (* Fixed: minor instead of minor_int *)
                | VMatrixFloat lst ->
                    if r < 0 || r >= List.length lst || c < 0 || c >= List.length (List.nth lst r) then
                      raise (RuntimeError ("Index out of bounds in matrix: " ^ m))
                    else
                      VMatrixFloat (minor_float lst r c), env2
                | _ -> raise (RuntimeError ("Invalid matrix index target: " ^ m)))
           | _ -> raise (RuntimeError ("Matrix indices must be integers in: " ^ string_of_expr expr)))