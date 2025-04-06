(*type dsl_type =
    | TInt
    | TFloat
    | TBool
    | TVectorInt of int
    | TVectorFloat of int
    | TMatrixInt of int * int
    | TMatrixFloat of int * int
   
    (* rows * cols *)

(*type all = int | bool | float | int*string | int*int*string
;;*)


type expr =
      | IntConst of int            
      | BoolConst of bool          
      | FloatConst of float        
      | Var of string            
      | Vector of int * string   
      | Matrix of int * int * string 
      
      
      | Plus of expr * expr      
      | Minus of expr * expr     
      | Times of expr * expr     
      | Div of expr * expr       
      | Mod of expr * expr       
      | Neg of expr              
      
      
      | Eq of expr * expr        
      | Neq of expr * expr       
      | LessThan of expr * expr  
      | GreaterThan of expr * expr 
      | LessEqual of expr * expr 
      | GreaterEqual of expr * expr 

      
      | Conj of expr * expr      
      | Disj of expr * expr      
      | Not of expr              
      
      
      
      | Dot of expr * expr       
      | Angle of expr * expr     
      | Transpose of expr        
      | Determinant of expr      

      (* Built-in Functions *)
      | Abs of expr              
      | Dim of expr     
      | Declare of dsl_type * string 
      | Assign of expr * expr
      | If of expr * expr * expr option
      | For of expr * expr * expr * expr
      | While of expr * expr

      

    
    
      | Input of string option  (* Now it accepts an optional argument *)
      | Print of string option
      | Seq of expr * expr
      | VectorIndex of string * expr
      | MatrixIndex of string * expr * expr
      



module StringMap = Map.Make(String)
let symbol_map : dsl_type StringMap.t = StringMap.empty;;



type alltypes = 
   Int
  | Bool
  | Float
  | Vectorint of int
  | Vectorfloat of int
  | Matrixint of int * int
  | Matrixfloat of int * int
  | Statement 
  | All

  let classify_vector_type s =
    let s = String.trim s in
    if String.length s > 2 && s.[0] = '[' && s.[String.length s - 1] = ']' then
      let inner = String.sub s 1 (String.length s - 2) |> String.trim in
      let elements = String.split_on_char ',' inner |> List.map String.trim in
      match elements with
      | first :: _ -> 
        (try 
          ignore (int_of_string first); "int"
        with Failure _ ->
          try 
            ignore (float_of_string first); "float"
          with Failure _ -> "unknown")
      | [] -> "empty"
    else
      "invalid"
  
  (* Type inference function *)
  let rec type_of exp symbol_map =
    match exp with
    | IntConst _ -> (Int, symbol_map)
    | BoolConst _ -> (Bool, symbol_map)
    | FloatConst _ -> (Float, symbol_map)
    | Var s -> 
        (try
          (StringMap.find s symbol_map, symbol_map)
        with Not_found -> raise (wrong exp))

        | Vector (n, s) -> 
          (match classify_vector_type s with  (* Fix: only pass s *)
           | "int" -> (Vectorint n, symbol_map)
           | "float" -> (Vectorfloat n, symbol_map)
           | _ -> failwith "Invalid vector type")
      | Matrix (m, n, s) -> 
          (match classify_vector_type s with  (* Fix: only pass s *)
           | "int" -> (Matrixint (m, n), symbol_map)
           | "float" -> (Matrixfloat (m, n), symbol_map)
           | _ -> failwith "Invalid matrix type")
    
    | Plus (e1, e2) | Minus (e1, e2) ->
        let t1 = type_of e1 symbol_map in
        let t2 = type_of e2 symbol_map in
        if t1 = Int && t2 = Int then (Int, symbol_map)
        else if t1 = Float && t2 = Float then (Float, symbol_map)
        else if t1 = Vectorint n && t2 = Vectorint m && n=m then (Vectorint n, symbol_map)
        else if t1 = Vectorfloat n && t2 = Vectorfloat m && n=m then (Vectorfloat n, symbol_map)
        else if t1 = Matrixint (m, n) && t2 = Matrixint (p, q) && m=p && n=q then (Matrixint (m, n), symbol_map)
        else if t1 = Matrixfloat (m, n) && t2 = Matrixfloat (p, q) && m=p && n=q then (Matrixfloat (m, n), symbol_map)
        else raise (wrong exp)
        
    | Dot(e1, e2) ->
        let t1 = type_of e1 symbol_map in
        let t2 = type_of e2 symbol_map in
        if t1 = Vectorint n && t2 = Vectorint m && n=m then (Int, symbol_map)
        else if t1 = Vectorfloat n && t2 = Vectorfloat m && n=m then (Float, symbol_map)
        else if t1 = Matrixint (m, n) && t2 = Matrixint (p, q) && m=p && n=q then (Int, symbol_map)
        else if t1 = Matrixfloat (m, n) && t2 = Matrixfloat (p, q) && m=p && n=q then (Float, symbol_map)
        else raise (wrong exp)
        
    | Angle(e1, e2) ->
        let t1 = type_of e1 symbol_map in
        let t2 = type_of e2 symbol_map in
        if t1 = Vectorint n && t2 = Vectorint m && n=m then (Int, symbol_map)
        else if t1 = Vectorfloat n && t2 = Vectorfloat m && n=m then (Float, symbol_map)
        else if t1 = Matrixint (m, n) && t2 = Matrixint (p, q) && m=p && n=q then (Int, symbol_map)
        else if t1 = Matrixfloat (m, n) && t2 = Matrixfloat (p, q) && m=p && n=q then (Float, symbol_map)
        else raise (wrong exp)
        


    | Div (e1, e2) | Mod (e1, e2) ->
        let t1 = type_of e1 symbol_map in
        let t2 = type_of e2 symbol_map in
        if t1 = Int && t2 = Int then (Int, symbol_map)
        else if t1 = Float && t2 = Float then (Float, symbol_map)
        else raise (wrong exp)
        
    | Times (e1, e2) ->
        let t1 = type_of e1 symbol_map in
        let t2 = type_of e2 symbol_map in
        if t1 = Int && t2 = Int then (Int, symbol_map)
        else if t1 = Float && t2 = Float then (Float, symbol_map)
        else if t1 = Int && t2 = Vectorint n then (Vectorint n, symbol_map)
        else if t1 = Float && t2 = Vectorfloat n then (Vectorfloat n, symbol_map)
        else if t1 = Int && t2 = Matrixint (m, n) then (Matrixint (m, n), symbol_map)
        else if t1 = Float && t2 = Matrixfloat (m, n) then (Matrixfloat (m, n), symbol_map)
        else raise (wrong exp)
        
    | Neg e ->
        let t = type_of e symbol_map in
        if t = Int then (Int, symbol_map)
        else if t = Float then (Float, symbol_map)
        else raise (wrong exp)
        

    | Eq (e1, e2) | Neq (e1, e2) | LessThan (e1, e2) | GreaterThan (e1, e2) | LessEqual (e1, e2) | GreaterEqual (e1, e2) ->
        let t1 = type_of e1 symbol_map in
        let t2 = type_of e2 symbol_map in
        if t1 = Int && t2 = Int then (Bool, symbol_map)
      
        else raise (wrong exp)
        
    
    | Conj (e1, e2) | Disj (e1, e2) ->
        let t1 = type_of e1 symbol_map in
        let t2 = type_of e2 symbol_map in
        if t1 = Bool && t2 = Bool then (Bool, symbol_map)
        else raise (wrong exp)
    | Not e ->
        let t = type_of e symbol_map in
        if t = Bool then (Bool, symbol_map)
        else raise (wrong exp)

    | Print e -> (Statement, symbol_map)
    
    | Assign (e1, e2) ->
        let t1 = type_of e1 symbol_map in
        let t2 = type_of e2 symbol_map in
        if t1 = t2  then (Statement, symbol_map)
        else if t2 = All then (Statement, symbol_map)
        else raise(wrong exp)
        
    | Transpose e ->
        let t = type_of e symbol_map in
        (match t with
         | Matrixint (m, n) -> (Matrixint (n, m), symbol_map)
         | Matrixfloat (m, n) -> (Matrixfloat (n, m), symbol_map)
         | _ -> raise (wrong exp))
    | Determinant e ->
        let t = type_of e symbol_map in
        (match t with
         | Matrixint (m, n) -> (Int, symbol_map)
         | Matrixfloat (m, n) -> (Float, symbol_map)
         | _ -> raise (wrong exp))
    | Abs e ->
        let t = type_of e symbol_map in
        if t = Int then (Int, symbol_map)
        else if t = Float then (Float, symbol_map)
        else raise (wrong exp)
    | Dim e ->
        let t = type_of e symbol_map in
        (match t with
         | Vectorint n -> (Int, symbol_map)
         | Vectorfloat n -> (Int, symbol_map)
         | Matrixint (m, n) -> (Int,  symbol_map)
         | Matrixfloat (m, n) -> (Int, symbol_map)
         | _ -> raise (wrong exp))
    | Declare (t, _) -> (t, symbol_map) (*check*)
    | Input(_) -> (All, symbol_map)

    | VectorIndex(v, i) -> 
        let t1 = type_of i symbol_map in
        let t2 = type_of (Var v) symbol_map in
        if t1 = Int && t2 = Vectorint n then (Int, symbol_map)
        else if t1 = Int && t2 = Vectorfloat n then (Float, symbol_map)
        else raise (wrong exp)
        
    
    | MatrixIndex(m,a,b) ->
      let t1 = type_of a symbol_map in
      let t2 = type_of b symbol_map in
      let t3 = type_of (Var m) symbol_map in
      if t1 = Int && t2 = Int && t3 = Matrixint (m, n) then (Int, symbol_map)
      else if t1 = Int && t2 = Int && t3 = Matrixfloat (m, n) then (Float, symbol_map)
      else raise (wrong exp)
    
    | If (e1, e2, e3) ->
        let t1 = type_of e1 symbol_map in
        let t2 = type_of e2 symbol_map in
        let t3 = match e3 with
          | Some e -> (type_of e, symbol_map)
          | None -> (All, symbol_map)
        in
        if t1 = Bool && (t2 = t3 || t3 = All) then (t2, symbol_map) (*check*)
        else raise (wrong exp)
    | For (e1, e2, e3, e4) ->
        let t1 = type_of e1 symbol_map in
        let t2 = type_of e2 symbol_map in
        let t3 = type_of e3 symbol_map in
        let t4 = type_of e4 symbol_map in
        (match t1, t2, t3, t4 with
          Assign(_, _), Bool, Assign(_, _), Statement -> (Statement, symbol_map)
        | _ -> raise (wrong exp))
    | While (e1, e2) ->
        let t1 = type_of e1 symbol_map in
        let t2 = type_of e2 symbol_map in
        if t1 = Bool && t2 = Statement then (Statement, symbol_map)
        else raise (wrong exp)
    | Seq(e1, e2) ->
        let t1 = type_of e1 symbol_map in
        let t2 = type_of e2 symbol_map in
        if t1 = Statement && t2 = Statement then (Statement, symbol_map)
        else raise (wrong exp)

    | _ -> raise (wrong exp)
  and wrong exp = Failure ("Wrong type: " ^ (string_of_expr exp))   *)


(* ast.ml *)


exception TypeError of string

type dsl_type =
  | TInt
  | TFloat
  | TBool
  | TVectorInt of int
  | TVectorFloat of int
  | TMatrixInt of int * int
  | TMatrixFloat of int * int

type expr =
  | IntConst of int            
  | BoolConst of bool          
  | FloatConst of float        
  | Var of string            
  | Vector of int * string   
  | Matrix of int * int * string 
  | Plus of expr * expr      
  | Minus of expr * expr     
  | Times of expr * expr     
  | Div of expr * expr       
  | Mod of expr * expr       
  | Neg of expr              
  | Eq of expr * expr        
  | Neq of expr * expr       
  | LessThan of expr * expr  
  | GreaterThan of expr * expr 
  | LessEqual of expr * expr 
  | GreaterEqual of expr * expr 
  | Conj of expr * expr      
  | Disj of expr * expr      
  | Not of expr              
  | Dot of expr * expr       
  | Angle of expr * expr   
  | Pow of expr * expr  
  | Transpose of expr        
  | Determinant of expr  
  | Minor of string*expr*expr    
  | Inverse of expr
  | Abs of expr              
  | Dim of expr     
  | Declare of dsl_type * string 
  | Assign of expr * expr
  | If of expr * expr * expr option
  | For of expr * expr * expr * expr
  | While of expr * expr
  | Input of string option  
  | Print of string option
  | Seq of expr * expr
  | VectorIndex of string * expr
  | MatrixIndex of string * expr * expr
  
  

type alltypes = 
  | Int
  | Bool
  | Float
  | Vectorint of int
  | Vectorfloat of int
  | Matrixint of int * int
  | Matrixfloat of int * int
  | Statement 
  | All

module StringMap = Map.Make(String)
let symbol_map : dsl_type StringMap.t = StringMap.empty

(* Helper function to convert dsl_type to alltypes *)
let dsl_to_alltypes = function
  | TInt -> Int
  | TFloat -> Float
  | TBool -> Bool
  | TVectorInt n -> Vectorint n
  | TVectorFloat n -> Vectorfloat n
  | TMatrixInt (m, n) -> Matrixint (m, n)
  | TMatrixFloat (m, n) -> Matrixfloat (m, n)

(* Classify vector/matrix string literal type *)
let classify_vector_type s =
  let s = String.trim s in
  if String.length s < 2 || s.[0] <> '[' || s.[String.length s - 1] <> ']' then
    "invalid"
  else
    let inner = String.sub s 1 (String.length s - 2) |> String.trim in
    if String.length inner > 0 && inner.[0] = '[' then
      (* Handle matrix case *)
      let rows = String.split_on_char ']' inner |> List.filter ((<>) "") |> List.map String.trim in
      let elements = List.map (fun row ->
        let cleaned = String.trim (if row.[0] = '[' then String.sub row 1 (String.length row - 1) else row) in
        String.split_on_char ',' cleaned |> List.map String.trim
      ) rows in
      let first_element = List.hd (List.hd elements) in
      try
        ignore (int_of_string first_element); "int"
      with Failure _ ->
        try
          ignore (float_of_string first_element); "float"
        with Failure _ -> "unknown"
    else
      (* Handle vector case *)
      let elements = String.split_on_char ',' inner |> List.map String.trim in
      match elements with
      | first :: _ ->
          (try
            ignore (int_of_string first); "int"
          with Failure _ ->
            try
              ignore (float_of_string first); "float"
            with Failure _ -> "unknown")
      | [] -> "empty"
(* String representation of expr for error messages *)
let rec string_of_expr = function
  | IntConst n -> string_of_int n
  | BoolConst b -> string_of_bool b
  | FloatConst f -> string_of_float f
  | Var s -> s
  | Vector (n, s) -> "vector(" ^ string_of_int n ^ ", " ^ s ^ ")"
  | Matrix (m, n, s) -> "matrix(" ^ string_of_int m ^ "," ^ string_of_int n ^ "," ^ s ^ ")"
  | Plus (e1, e2) -> "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Minus (e1, e2) -> "(" ^ string_of_expr e1 ^ " - " ^ string_of_expr e2 ^ ")"
  | Times (e1, e2) -> "(" ^ string_of_expr e1 ^ " * " ^ string_of_expr e2 ^ ")"
  | Div (e1, e2) -> "(" ^ string_of_expr e1 ^ " / " ^ string_of_expr e2 ^ ")"
  | Mod (e1, e2) -> "(" ^ string_of_expr e1 ^ " % " ^ string_of_expr e2 ^ ")"
  | Neg e -> "(-" ^ string_of_expr e ^ ")"
  | Eq (e1, e2) -> "(" ^ string_of_expr e1 ^ " == " ^ string_of_expr e2 ^ ")"
  | Neq (e1, e2) -> "(" ^ string_of_expr e1 ^ " != " ^ string_of_expr e2 ^ ")"
  | LessThan (e1, e2) -> "(" ^ string_of_expr e1 ^ " < " ^ string_of_expr e2 ^ ")"
  | GreaterThan (e1, e2) -> "(" ^ string_of_expr e1 ^ " > " ^ string_of_expr e2 ^ ")"
  | LessEqual (e1, e2) -> "(" ^ string_of_expr e1 ^ " <= " ^ string_of_expr e2 ^ ")"
  | GreaterEqual (e1, e2) -> "(" ^ string_of_expr e1 ^ " >= " ^ string_of_expr e2 ^ ")"
  | Conj (e1, e2) -> "(" ^ string_of_expr e1 ^ " && " ^ string_of_expr e2 ^ ")"
  | Disj (e1, e2) -> "(" ^ string_of_expr e1 ^ " || " ^ string_of_expr e2 ^ ")"
  | Not e -> "(!" ^ string_of_expr e ^ ")"
  | Dot (e1, e2) -> "(" ^ string_of_expr e1 ^ " . " ^ string_of_expr e2 ^ ")"
  | Angle (e1, e2) -> "angle(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Pow (e1, e2) -> "(" ^ string_of_expr e1 ^ " ^ " ^ string_of_expr e2 ^ ")"
  | Transpose e -> "transpose(" ^ string_of_expr e ^ ")"
  | Determinant e -> "det(" ^ string_of_expr e ^ ")"
  | Minor (m,e1,e2) -> "minor(" ^ m ^ ", " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  (* | Minor (m, e1, e2) -> "minor(" ^ m ^ ", " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")" *)
  | Inverse e -> "inv(" ^ string_of_expr e ^ ")"
  | Abs e -> "abs(" ^ string_of_expr e ^ ")"
  | Dim e -> "dim(" ^ string_of_expr e ^ ")"
  | Declare (t, s) -> "declare(" ^ (match t with TInt -> "int" | TFloat -> "float" | TBool -> "bool" | TVectorInt n -> "vectorint " ^ string_of_int n | TVectorFloat n -> "vectorfloat " ^ string_of_int n | TMatrixInt (m, n) -> "matrixint " ^ string_of_int m ^ "," ^ string_of_int n | TMatrixFloat (m, n) -> "matrixfloat " ^ string_of_int m ^ "," ^ string_of_int n) ^ ", " ^ s ^ ")"
  | Assign (e1, e2) -> "(" ^ string_of_expr e1 ^ " = " ^ string_of_expr e2 ^ ")"
  | If (e1, e2, e3_opt) -> "if(" ^ string_of_expr e1 ^ ") " ^ string_of_expr e2 ^ (match e3_opt with Some e3 -> " else " ^ string_of_expr e3 | None -> "")
  | For (e1, e2, e3, e4) -> "for(" ^ string_of_expr e1 ^ "; " ^ string_of_expr e2 ^ "; " ^ string_of_expr e3 ^ ") " ^ string_of_expr e4
  | While (e1, e2) -> "while(" ^ string_of_expr e1 ^ ") " ^ string_of_expr e2
  | Input s_opt -> "input(" ^ (match s_opt with Some s -> s | None -> "") ^ ")"
  | Print s_opt -> "print(" ^ (match s_opt with Some s -> s | None -> "") ^ ")"
  | Seq (e1, e2) -> string_of_expr e1 ^ "; " ^ string_of_expr e2
  | VectorIndex (v, i) -> v ^ "[" ^ string_of_expr i ^ "]"
  | MatrixIndex (m, a, b) -> m ^ "[" ^ string_of_expr a ^ "][" ^ string_of_expr b ^ "]"
  

(* String representation of alltypes for printing *)
let string_of_alltypes = function
  | Int -> "Int"
  | Bool -> "Bool"
  | Float -> "Float"
  | Vectorint n -> "Vectorint " ^ string_of_int n
  | Vectorfloat n -> "Vectorfloat " ^ string_of_int n
  | Matrixint (m, n) -> "Matrixint " ^ string_of_int m ^ "x" ^ string_of_int n
  | Matrixfloat (m, n) -> "Matrixfloat " ^ string_of_int m ^ "x" ^ string_of_int n
  | Statement -> "Statement"
  | All -> "All"

(* Type inference function *)
let rec type_of exp symbol_map =
  match exp with
  | IntConst _ -> (Int, symbol_map)
  | BoolConst _ -> (Bool, symbol_map)
  | FloatConst _ -> (Float, symbol_map)
  | Var s -> 
      (try
        (dsl_to_alltypes (StringMap.find s symbol_map), symbol_map)
      with Not_found -> raise (wrong exp))
  
  | Vector (n, s) -> 
      (match classify_vector_type s with
       | "int" -> (Vectorint n, symbol_map)
       | "float" -> (Vectorfloat n, symbol_map)
       | _ -> raise (wrong exp))
  | Matrix (m, n, s) -> 
      (match classify_vector_type s with
       | "int" -> (Matrixint (m, n), symbol_map)
       | "float" -> (Matrixfloat (m, n), symbol_map)
       | _ -> raise (wrong exp))
  | Plus (e1, e2) | Minus (e1, e2) ->
      let (t1, sm1) = type_of e1 symbol_map in
      let (t2, sm2) = type_of e2 sm1 in
      if t1 = Int && t2 = Int then (Int, sm2)
      else if t1 = Float && t2 = Float then (Float, sm2)
      else if t1 = Float &&  t2 = Int then (Float, sm2)
      else if t1 = Int && t2 = Float then (Float , sm2)
      else if (match t1, t2 with Vectorint n, Vectorint m when n = m -> true | _ -> false) then (t1, sm2)
      else if (match t1, t2 with Vectorfloat n, Vectorfloat m when n = m -> true | _ -> false) then (t1, sm2)
      else if (match t1, t2 with Matrixint (m, n), Matrixint (p, q) when m = p && n = q -> true | _ -> false) then (t1, sm2)
      else if (match t1, t2 with Matrixfloat (m, n), Matrixfloat (p, q) when m = p && n = q -> true | _ -> false) then (t1, sm2)
      
      else raise (wrong exp)
  | Times (e1, e2) ->
      let (t1, sm1) = type_of e1 symbol_map in
      let (t2, sm2) = type_of e2 sm1 in
      if t1 = Int && t2 = Int then (Int, sm2)
      else if t1 = Float && t2 = Float then (Float, sm2)
      else if t1 = Int && t2 = Float then (Float , sm2)
      else if t1 = Float && t2 = Int then (Float , sm2)
      
      else if t1 = Int && (match t2 with Vectorint n -> true | _ -> false) then (t2, sm2)
      else if t1 = Float && (match t2 with Vectorfloat n -> true | _ -> false) then (t2, sm2)
      else if t1 = Int && (match t2 with Matrixint (m, n) -> true | _ -> false) then (t2, sm2)
      else if t1 = Float && (match t2 with Matrixfloat (m, n) -> true | _ -> false) then (t2, sm2)


      else raise (wrong exp)
  | Div (e1, e2) | Mod (e1, e2) ->
      let (t1, sm1) = type_of e1 symbol_map in
      let (t2, sm2) = type_of e2 sm1 in
      if t1 = Int && t2 = Int then (Int, sm2)
      else if t1 = Float && t2 = Float then (Float, sm2)
      else raise (wrong exp)
  | Neg e ->
      let (t, sm) = type_of e symbol_map in
      if t = Int then (Int, sm)
      else if t = Float then (Float, sm)
      else raise (wrong exp)
  | Eq (e1, e2) | Neq (e1, e2) | LessThan (e1, e2) | GreaterThan (e1, e2) | LessEqual (e1, e2) | GreaterEqual (e1, e2) ->
      let (t1, sm1) = type_of e1 symbol_map in
      let (t2, sm2) = type_of e2 sm1 in
      if t1 = t2 && t1 = Int then (Bool, sm2)
      else if t1 = t2 && t1 = Float then (Bool, sm2)
      else raise (wrong exp)
  | Conj (e1, e2) | Disj (e1, e2) ->
      let (t1, sm1) = type_of e1 symbol_map in
      let (t2, sm2) = type_of e2 sm1 in
      if t1 = Bool && t2 = Bool then (Bool, sm2)
      else raise (wrong exp)
  | Not e ->
      let (t, sm) = type_of e symbol_map in
      if t = Bool then (Bool, sm)
      else raise (wrong exp)
  | Dot (e1, e2) ->
    let (t1, sm1) = type_of e1 symbol_map in
    let (t2, sm2) = type_of e2 sm1 in
    if t1 = t2 then
      match t1 with
      | Vectorint n ->
          (Int, sm2)
      | Vectorfloat n ->
          (Float, sm2)
      | Matrixint (m, n) ->
          (match t2 with
            | Matrixint (p, q) ->
                if n = p then (Matrixint (m, q), sm2)
                else raise (TypeError "Wrong type: Matrix mult of incompatible matrices")
            | _ -> raise (wrong exp))
      | Matrixfloat (m, n) ->
          (match t2 with
            | Matrixfloat (p, q) ->
                if n = p then (Matrixfloat (m, q), sm2)
                else raise (TypeError "Wrong type: Matrix mult of incompatible matrices")
            | _ -> raise (wrong exp))
      | _ -> raise (wrong exp)
    else
      (* Now we give more specific errors based on combinations *)
      (match t1, t2 with
      | Vectorint n, Matrixint (p, q) ->
        if n = p then (Vectorint q, sm2)
        else raise (TypeError "Wrong type: Vector × Matrix shape mismatch")
    | Matrixint (m, n), Vectorint p ->
        if n = p then (Vectorint m, sm2)
        else raise (TypeError "Wrong type: Matrix × Vector shape mismatch")
    | Vectorfloat n, Matrixfloat (p, q) ->
        if n = p then (Vectorfloat q, sm2)
        else raise (TypeError "Wrong type: Vector × Matrix shape mismatch")
    | Matrixfloat (m, n), Vectorfloat p ->
        if n = p then (Vectorfloat m, sm2)
        else raise (TypeError "Wrong type: Matrix × Vector shape mismatch")
    | Vectorint _, Vectorint _ | Vectorfloat _, Vectorfloat _ ->
        raise (TypeError "Wrong type: Dot product of incompatible vectors")
    | Matrixint _, Matrixint _ | Matrixfloat _, Matrixfloat _ ->
        raise (TypeError "Wrong type: Matrix mult of incompatible matrices")
    | _ -> raise (wrong exp))
  | Angle (e1, e2) ->
      let (t1, sm1) = type_of e1 symbol_map in
      let (t2, sm2) = type_of e2 sm1 in
      if (match t1, t2 with Vectorint n, Vectorint m when n = m -> true | _ -> false) then (Float, sm2)
      else if (match t1, t2 with Vectorfloat n, Vectorfloat m when n = m -> true | _ -> false) then (Float, sm2)
      else raise (wrong exp)
  | Pow(e1, e2) ->
      let (t1, sm1) = type_of e1 symbol_map in
      let (t2, sm2) = type_of e2 sm1 in
      if t1 = Int && t2 = Int then (Int, sm2)
      else if t1 = Float && t2 = Float then (Float, sm2)
      else if t1 = Int && t2 = Float then (Float, sm2)
      else if t1 = Float && t2 = Int then (Float, sm2)
      else raise (wrong exp)
  | Transpose e ->
      let (t, sm) = type_of e symbol_map in
      (match t with
       | Matrixint (m, n) -> (Matrixint (n, m), sm)
       | Matrixfloat (m, n) -> (Matrixfloat (n, m), sm)
       | _ -> raise (wrong exp))
  | Determinant e ->
    
    let (t, sm) = type_of e symbol_map in
    
    (match t with
     | Matrixint (m, n) ->
         if m = n then (Int, sm)
         else raise (TypeError ("Wrong type: Determinant of non-square matrix: " ^ string_of_expr e))
     | Matrixfloat (m, n) when m = n -> (Float, sm)
     | Matrixfloat (m, n) when m <> n -> raise (TypeError ("Wrong type: Determinant of non-square matrix: " ^ string_of_expr e))
     | _ -> raise (TypeError ("Wrong type: Determinant expects a matrix: " ^ string_of_expr e)))
  | Minor(m, e1, e2)->
      let (t1, sm1) = type_of e1 symbol_map in
      let (t2, sm2) = type_of e2 sm1 in
      let (t3, sm3) = type_of (Var m) sm2 in
      if t1 = Int && t2 = Int then
        match t3 with
        | Matrixint (m, n) -> (Matrixint (m-1, n-1), sm3)
        | Matrixfloat (m, n) -> (Matrixfloat (m-1, n-1), sm3)
        | _ -> raise (wrong exp)
      else raise (wrong exp)
  | Inverse e ->
      let (t, sm) = type_of e symbol_map in
      (match t with
       | Matrixint (m, n) ->
           if m = n then (Matrixint (n, m), sm)
           else raise (TypeError ("Wrong type: Inverse of non-square matrix: " ^ string_of_expr e))
       | Matrixfloat (m, n) when m = n -> (Matrixfloat (n, m), sm)
       | Matrixfloat (m, n) when m <> n -> raise (TypeError ("Wrong type: Inverse of non-square matrix: " ^ string_of_expr e))
       | _ -> raise (TypeError ("Wrong type: Inverse expects a matrix: " ^ string_of_expr e)))
  | Abs e ->
      let (t, sm) = type_of e symbol_map in
      if t = Int then (Int, sm)
      else if t = Float then (Float, sm)
      else raise (wrong exp)
  | Dim e ->
      let (t, sm) = type_of e symbol_map in
      (match t with
       | Vectorint _ | Vectorfloat _ | Matrixint _ | Matrixfloat _ -> (Int, sm)
       | _ -> raise (wrong exp))
  | Declare (t, s) ->
      let new_map = StringMap.add s t symbol_map in
      (Statement, new_map)
  | Assign (e1, e2) ->
      let (t1, sm1) = type_of e1 symbol_map in
      let (t2, sm2) = type_of e2 sm1 in
      (match e1 with
        | Var x -> 
          (let t1 = dsl_to_alltypes (StringMap.find x symbol_map) in
          if t1 = t2 || t2= All then (Statement, sm2)
          else raise (wrong exp))
        | Declare(t, x) -> 
          (match t, t2 with
            TInt, Int -> (Statement, sm2)
          | TFloat, Float -> (Statement, sm2)
          | TBool, Bool -> (Statement, sm2)
          | TVectorInt n, Vectorint m when n = m -> (Statement, sm2)
          | TVectorFloat n, Vectorfloat m when n = m -> (Statement, sm2)
          | TMatrixInt (m, n), Matrixint (p, q) when m = p && n = q -> (Statement, sm2)
          | TMatrixFloat (m, n), Matrixfloat (p, q) when m = p && n = q -> (Statement, sm2)
          | _, All -> (Statement, sm2)
          | _ -> raise (wrong exp)
          )
        | MatrixIndex(m, a, b) -> 
          let (t1, sm1_a) = type_of a sm1 in  (* Destructure t1 *)
          let (t2_idx, sm2_idx) = type_of b sm1_a in  (* Rename t2 to avoid shadowing *)
          let (t3, sm3) = type_of (Var m) sm2_idx in
          if t1 = Int && t2_idx = Int then
            (match
              t3 with
              | Matrixint (m, n) -> (Statement, sm3)
              | Matrixfloat (m, n) -> (Statement, sm3)
              | _ -> raise (wrong exp))
          else raise (wrong exp)
        | VectorIndex(v, i) -> 
          let (t1, sm1) = type_of i sm1 in
          let (t2, sm2) = type_of (Var v) sm1 in
          if t1 = Int && (match t2 with Vectorint _ -> true | Vectorfloat _ -> true | _ -> false) then
            (match t2 with Vectorint _ -> (Statement, sm2) | Vectorfloat _ -> (Statement, sm2) | _ -> assert false)
          else raise (wrong exp)
        (*| MatrixIndex(m,a,b) ->
          let t1 = type_of a symbol_map in
          let t2 = type_of b symbol_map in
          let t3 = type_of (Var m) symbol_map in
          if t1 = Int && t2 = Int && t3 = Matrixint (m, n) then (Int, symbol_map)
          else if t1 = Int && t2 = Int && t3 = Matrixfloat (m, n) then (Float, symbol_map)
          else raise (wrong exp)*)
        (*| If(e1,e2,e3_opt) ->
          let t1 = type_of e1 symbol_map in
          let t2 = type_of e2 symbol_map in
          let t3 = match e3_opt with Some e -> type_of e symbol_map | None -> All in
          if t1 = Bool && (t2 = t3 || t3=All) then (t2, symbol_map)
          else raise (wrong exp)*)
        | _ -> raise (wrong exp))
       
  | If (e1, e2, e3_opt) ->
      let (t1, sm1) = type_of e1 symbol_map in
      let (t2, sm2) = type_of e2 sm1 in
      if t1 <> Bool then raise (wrong exp);
      (match e3_opt with
       | Some e3 ->
           let (t3, sm3) = type_of e3 sm2 in
           if t2 = t3 then (t2, sm1)
           else raise (wrong exp)
       | None -> (Statement, sm1))
  | For (e1, e2, e3, e4) ->
      
      let (_, sm1) = type_of e1 symbol_map in
      let (t2, sm2) = type_of e2 sm1 in
      let (_, sm3) = type_of e3 sm2 in
      let (t4, sm4) = type_of e4 sm3 in
     
      if t2 = Bool && t4 = Statement then (Statement, sm1)

      else raise (wrong exp)
  | While (e1, e2) ->
      let (t1, sm1) = type_of e1 symbol_map in
      let (t2, sm2) = type_of e2 sm1 in
      if t1 = Bool && t2 = Statement then (Statement, sm1)
      else raise (wrong exp)
  | Input _ -> (All, symbol_map)
  | Print _ -> (Statement, symbol_map)
  | Seq (e1, e2) ->
      let (_, sm1) = type_of e1 symbol_map in
      let (t2, sm2) = type_of e2 sm1 in
      (t2, sm2)
  | VectorIndex (v, i) ->
    let (t1, sm1) = type_of i symbol_map in
    let (t2, sm2) = type_of (Var v) sm1 in
    if t1 = Int then (
      match t2 with
      | Vectorint _ -> (Int, sm2)
      | Vectorfloat _ -> (Float, sm2)
      | Matrixint (_, a) -> (Vectorint a, sm2)
      | Matrixfloat (_, b) -> (Vectorfloat b, sm2)
      | _ -> raise (wrong exp)
    )
    else raise (wrong exp)

  | MatrixIndex (m, a, b) ->
    let (t1, sm1) = type_of a symbol_map in
    let (t2, sm2) = type_of b sm1 in
    let (t3, sm3) = type_of (Var m) sm2 in
    (match t3 with
    | Matrixint (_, _) when t1 = Int && t2 = Int -> (Int, sm3)
    | Matrixfloat (_, _) when t1 = Int && t2 = Int -> (Float, sm3)
    | _ -> raise (wrong exp))

  


(* Error helper *)
and wrong exp =
  Failure ("Wrong type for expression: " ^ string_of_expr exp)

(* Main function to run and print results *)
(* Main function with complicated example *)
(* Main function with complicated example *)
(*let main () =
  let initial_map = symbol_map in
  
  (* int x; x = 5 *)
  let decl_x = Declare (TInt, "x") in
  let assign_x = Assign (Var "x", IntConst 5) in
  let (_, sm1) = type_of decl_x initial_map in
  let (_, sm2) = type_of assign_x sm1 in
  print_endline ("Type of " ^ string_of_expr decl_x ^ ": " ^ string_of_alltypes Statement);
  print_endline ("Type of " ^ string_of_expr assign_x ^ ": " ^ string_of_alltypes Statement);

  (* float[3] v1 = [1.0, 2.0, 3.0] *)
  let decl_v1 = Declare (TVectorFloat 3, "v1") in
  let assign_v1 = Assign (Var "v1", Vector (3, "[1.0, 2.0, 3.0]")) in
  let (_, sm3) = type_of decl_v1 sm2 in
  let (_, sm4) = type_of assign_v1 sm3 in
  print_endline ("Type of " ^ string_of_expr decl_v1 ^ ": " ^ string_of_alltypes Statement);
  print_endline ("Type of " ^ string_of_expr assign_v1 ^ ": " ^ string_of_alltypes Statement);

  (* float[3] v2 = [4.0, 5.0, 6.0] *)
  let decl_v2 = Declare (TVectorFloat 3, "v2") in
  let assign_v2 = Assign (Var "v2", Vector (3, "[4.0, 5.0, 6.0]")) in
  let (_, sm5) = type_of decl_v2 sm4 in
  let (_, sm6) = type_of assign_v2 sm5 in
  print_endline ("Type of " ^ string_of_expr decl_v2 ^ ": " ^ string_of_alltypes Statement);
  print_endline ("Type of " ^ string_of_expr assign_v2 ^ ": " ^ string_of_alltypes Statement);

  (* float[2][3] m = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]] *)
  let decl_m = Declare (TMatrixFloat (2, 3), "m") in
  let assign_m = Assign (Var "m", Matrix (2, 3, "[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]")) in
  let (_, sm7) = type_of decl_m sm6 in
  let (_, sm8) = type_of assign_m sm7 in
  print_endline ("Type of " ^ string_of_expr decl_m ^ ": " ^ string_of_alltypes Statement);
  print_endline ("Type of " ^ string_of_expr assign_m ^ ": " ^ string_of_alltypes Statement);

  (* float dot_result = 0.0 *)
  let decl_dot = Declare (TFloat, "dot_result") in
  let assign_dot = Assign (Var "dot_result", FloatConst 0.0) in
  let (_, sm9) = type_of decl_dot sm8 in
  let (_, sm10) = type_of assign_dot sm9 in
  print_endline ("Type of " ^ string_of_expr decl_dot ^ ": " ^ string_of_alltypes Statement);
  print_endline ("Type of " ^ string_of_expr assign_dot ^ ": " ^ string_of_alltypes Statement);

  (* for (x = 0; x < 3; x = x + 1) { if (x < 2) { dot_result = dot_result + (v1[x] * v2[x]); m[x][x] = v1[x] + v2[x]; } else { float[3] temp = v1 + v2; dot_result = dot(v1, temp); } } *)
  let for_init = Assign (Var "x", IntConst 0) in
  let for_cond = LessThan (Var "x", IntConst 3) in
  let for_incr = Assign (Var "x", Plus (Var "x", IntConst 1)) in
  let if_cond = LessThan (Var "x", IntConst 2) in
  let if_then = Seq (
    Assign (Var "dot_result", Plus (Var "dot_result", Times (VectorIndex ("v1", Var "x"), VectorIndex ("v2", Var "x")))),
    Assign (MatrixIndex ("m", Var "x", Var "x"), Plus (VectorIndex ("v1", Var "x"), VectorIndex ("v2", Var "x")))
  ) in
  let if_else = Seq (
    Seq (
      Declare (TVectorFloat 3, "temp"),
      Assign (Var "temp", Plus (Var "v1", Var "v2"))
    ),
    Assign (Var "dot_result", Dot (Var "v1", Var "temp"))
  ) in
  let for_body = If (if_cond, if_then, Some if_else) in
  let for_loop = For (for_init, for_cond, for_incr, for_body) in
  let (_, sm11) = type_of for_loop sm10 in
  print_endline ("Type of " ^ string_of_expr for_loop ^ ": " ^ string_of_alltypes Statement);

  (* float det_m = det(transpose(m)) *)
  let decl_det_m = Declare (TFloat, "det_m") in
  let assign_det_m = Assign (Var "det_m", Determinant (Transpose (Var "m"))) in
  let (_, sm12) = type_of decl_det_m sm11 in
  let (_, sm13) = type_of assign_det_m sm12 in
  print_endline ("Type of " ^ string_of_expr decl_det_m ^ ": " ^ string_of_alltypes Statement);
  print_endline ("Type of " ^ string_of_expr assign_det_m ^ ": " ^ string_of_alltypes Statement);

  (* float final_result = dot_result + det_m *)
  let decl_final = Declare (TFloat, "final_result") in
  let assign_final = Assign (Var "final_result", Plus (Var "dot_result", Var "det_m")) in
  let (_, sm14) = type_of decl_final sm13 in
  let (_, sm15) = type_of assign_final sm14 in
  print_endline ("Type of " ^ string_of_expr decl_final ^ ": " ^ string_of_alltypes Statement);
  print_endline ("Type of " ^ string_of_expr assign_final ^ ": " ^ string_of_alltypes Statement);

  (* Print the type of the final expression *)
  let final_expr = Plus (Var "dot_result", Var "det_m") in
  let (final_typ, _) = type_of final_expr sm15 in
  print_endline ("Type of " ^ string_of_expr final_expr ^ ": " ^ string_of_alltypes final_typ)

(* Execute main *)
let () = main ()
let () = main ()*)