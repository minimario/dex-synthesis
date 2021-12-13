open Core;;

type var = string

type dextype = 
| VarType of var
| ArrowType of dextype * dextype
| Error

let dim_dextype dextype = 
  let rec go = function 
  | VarType v -> 1
  | ArrowType (t1, t2) -> go t1 + go t2
  | Error -> -1000
  in go dextype - 1

let rec equal_dextype o1 o2 = 
  match o1, o2 with 
  | VarType s1, VarType s2 -> equal_string s1 s2
  | ArrowType (d1, d2), ArrowType (d3, d4) -> equal_dextype d1 d3 && equal_dextype d2 d4
  | Error, Error -> true
  | _ -> false

type expr = 
| Var of var
| Dot of expr * var
| Sum of expr
| For of var * expr
| BinProd of expr * expr

let rec expr_to_string = function 
  | Var v -> v
  | Dot (e, v) -> (expr_to_string e) ^ "." ^ v
  | Sum expr -> "sum (" ^ (expr_to_string expr) ^ ")"
  | For (v, e) -> "(for " ^ v ^ "." ^ (expr_to_string e) ^ ")"
  | BinProd (e1, e2) -> "(" ^ (expr_to_string e1) ^ ")" ^ "*" ^ "(" ^ (expr_to_string e2) ^ ")"

let rec expr_to_python_tuple = function 
  | Var v -> Printf.sprintf "('Var', '%s')" v
  | Dot (e, v) -> Printf.sprintf "('Dot', %s, '%s')" (expr_to_python_tuple e) v
  | Sum expr -> Printf.sprintf "('Sum', %s)" (expr_to_python_tuple expr)
  | For (v, e) -> Printf.sprintf "('For', '%s', %s)" v (expr_to_python_tuple e)
  | BinProd (e1, e2) -> Printf.sprintf "('BinProd', %s, %s)" (expr_to_python_tuple e1) (expr_to_python_tuple e2)

let compare_expr e1 e2 = 
  let s1 = expr_to_string e1 in
  let s2 = expr_to_string e2 in
  String.compare s1 s2

let rec type_to_string = function
| VarType v -> v
| ArrowType (t1, t2) -> (type_to_string t1) ^ " => " ^ (type_to_string t2)
| Error -> "Error" 

let print_ctx ctx = Map.iteri ctx ~f:(fun ~key:v ~data:t -> (print_string (v ^ " : " ^ (type_to_string t) ^ "\n")))

(* returns (correct type, augmented context) *)
let rec typecheck expr ctx = match expr with
| Var v -> (match Map.find ctx v with 
  | Some t -> (t, ctx)
  | None -> (Error, Map.empty (module String)))
| Dot (e, v) -> 
  (let (t, ctx') = typecheck e ctx in
  (match t with 
  | VarType _ | Error -> (Error, Map.empty (module String))
  | ArrowType (t1, t2) -> 
    let new_ctx = Map.set ctx' ~key:v ~data:t1 in
    (t2, new_ctx)
  ))
| For (v, e) -> 
  let (t, ctx') = typecheck e ctx in
  let restype = match Map.find ctx' v, t with
  | _, Error | None, _ -> Error
  | Some vtype, sometype -> ArrowType (vtype, sometype)
  in 
  if equal_dextype restype Error then (Error, Map.empty (module String)) else (restype, ctx')
| Sum e -> 
  let (t, ctx') = typecheck e ctx in
  let restype = match t with 
  | ArrowType (VarType v1, VarType v2) -> VarType v2
  | ArrowType _ | VarType _ | Error -> Error
  in
  if equal_dextype restype Error then (Error, Map.empty (module String)) else (restype, ctx')
| BinProd (e1, e2) ->
  let (t1, ctx1) = typecheck e1 ctx in
  let (t2, ctx2) = typecheck e2 ctx in
  let ctx' = Map.merge_skewed ctx1 ctx2 ~combine:(fun ~key v1 v2 -> v1) in
  match t1, t2 with
  | (VarType v1, VarType v2) when equal_string v1 v2 -> (VarType v1, ctx')
  | _ -> (Error, Map.empty (module String))

(* sum (for i. (x.i * y.i)) *)
(* gen_exprs (depth 5) (output dim 0) *)
(* sum -> gen_exprs (depth 4) (output dim 1) *)
(* for -> gen_exprs (depth 3) (output dim 0) (indices x4) *)
(* binprod ->  *)

let rec list_to_string = function 
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ ", " ^ list_to_string xs

let rec gen_exprs ctx inputs depth indices output_dim = 
  if depth <= 0 then [] 
  else if depth = 1 then 
    if List.length indices = 0 then inputs
     |> List.filter ~f:(function | Var v -> dim_dextype (String.Map.find_exn ctx v) = output_dim | _ -> false)
    else []
  else if output_dim = -1 then []   
  else (
    let prev_exprs = gen_exprs ctx inputs (depth - 1) indices (output_dim + 1) in
    let sum_exprs = List.map ~f:(fun expr -> Sum expr) prev_exprs in
    let dot_exprs = 
      List.map ~f:(fun ivar -> 
        List.map ~f:(fun expr -> Dot (expr, ivar)) 
          (gen_exprs ctx inputs (depth - 1) (List.filter ~f:(fun x -> not (equal_string x ivar)) indices) (output_dim + 1))
      ) indices |> List.concat in
    (* let dot_exprs = List.map ~f:(fun expr -> List.map ~f:(fun ivexprar -> Dot (expr, ivar)) indices) prev_exprs |> List.concat in *)

    let varname = "x" ^ string_of_int depth in
    let prev_exprs_with_var = 
      gen_exprs ctx inputs (depth - 1) (varname::indices) (output_dim - 1)
      (* |> List.filter ~f:(fun x -> match x with | Dot (_, varname) -> false | _ -> true) *)
    in
    let for_exprs = 
      List.map ~f:(fun expr -> For (varname, expr)) prev_exprs_with_var in
    
    let prod_exprs = 
      if output_dim <> 0 then []
      else 
        let prev_prod_exprs = gen_exprs ctx inputs (depth - 3) indices 0 in
      List.map ~f:(fun expr -> 
        List.map ~f:(fun expr' -> 
          if compare_expr expr expr' >= 0 then (BinProd (expr, expr')) else (Var "invalid")
          ) prev_prod_exprs
      ) prev_prod_exprs
      |> List.concat
      |> List.filter ~f:(fun x -> match x with | Var "invalid" -> false | _ -> true)
    in
    
    let all_exprs = List.concat [sum_exprs; dot_exprs; for_exprs; prod_exprs] |> 
      List.filter ~f:(fun expr -> typecheck expr ctx |> fst |> equal_dextype Error |> not) 
    in
    (* let _= ( *)
      (* if (List.length all_exprs <> 0) then  *)
        (* let _ = print_string ("gen_exprs " ^ string_of_int depth ^ " " ^ string_of_int output_dim ^  " " ^ list_to_string indices ^ "\n") in  *)
        (* List.iter ~f:(fun expr -> print_string ("  " ^ (expr_to_string expr) ^ "\n")) all_exprs *)
      (* else ()  *)
    (* ) in  *)
    all_exprs
  )

(* let sample_expr' = For ("x9", For ("x8", For ("x7", For ("x4", Dot (Dot (Dot (Dot (Var "xs", "x4"), "x9"), "x8"), "x7"))))) *)
(* let sample_expr = For ("k", For ("i", For ("j", Dot (Dot (Dot (Var "xs", "j"), "k"), "i")))) *)

(* transpose *)
let input_type_1 = ArrowType (VarType "m", ArrowType (VarType "n", VarType "v"))
let output_type_1 = ArrowType (VarType "n", ArrowType (VarType "m", VarType "v"))
let ctx_1 = String.Map.singleton "xs" input_type_1

(* transpose first 2 dimensions of a 3d array *)
let input_type_2 = ArrowType (VarType "n", ArrowType (VarType "m", ArrowType (VarType "p", VarType "v")))
let output_type_2 = ArrowType (VarType "m", ArrowType (VarType "n", ArrowType (VarType "p", VarType "v")))
let ctx_2 = String.Map.singleton "xs" input_type_2

(* 2d matrix sum *)
let input_type_3 = ArrowType (VarType "n", ArrowType (VarType "m", VarType "v"))
let output_type_3 = VarType "v"
let ctx_3 = String.Map.singleton "xs" input_type_3

(* dot product *)
let input_1_type_4 = ArrowType (VarType "n", VarType "v")
let input_2_type_4 = ArrowType (VarType "n", VarType "v")
let output_type_4 = VarType "v"
let ctx_4 = String.Map.of_alist_exn [("x", input_1_type_4); ("y", input_2_type_4)]

(* matrix multiplication *)
(* let input_1_type_5 = ArrowType (VarType "n", ArrowType (VarType "m", VarType "v"))
let input_2_type_5 = ArrowType (VarType "m", ArrowType (VarType "p", VarType "v"))
let output_type_5 = ArrowType (VarType "n", ArrowType (VarType "p", VarType "v"))
let ctx_5 = String.Map.of_alist_exn [("x", input_1_type_5); ("y", input_2_type_5)] *)

let depth = 10
let ctx = ctx_1 (* change me! *)
let inputs = String.Map.keys ctx |> List.map ~f:(fun i -> Var i)
let output_type = output_type_1 (* change me! *)
let exprs = List.init ~f:(fun i -> gen_exprs ctx inputs (i+1) [] (dim_dextype output_type)) depth |> List.concat


let _ = 
  Py.initialize ();
  let _ = Py.Import.add_module "interpreter" in
  (* Py.Run.eval ~start:Py.File  *)
  let run_expr dex_expr expr = 
    Py.Run.eval ~start:Py.File (Printf.sprintf "
import copy

dex_expr = '%s'
expr = %s

ctx = {'xs': [[1, 2, 3], [4, 5, 6], [7, 8, 9]]}
values = {f'x{i}': 3 for i in range(20)}
var_values = {}
def interpret(expr, ctx, values, var_values):
    if expr[0] == 'Sum':
        return sum(interpret(expr[1], ctx, values, var_values))
    elif expr[0] == 'Var':
        return ctx[expr[1]]
    elif expr[0] == 'BinProd':
        return interpret(expr[1], ctx, values, var_values) * interpret(expr[2], ctx, values, var_values)
    elif expr[0] == 'Dot':
        body = interpret(expr[1], ctx, values, var_values)
        index = var_values[expr[2]]
        return body[index]
    elif expr[0] == 'For':
        res = []
        for i in range(values[expr[1]]):
            new_var_values = copy.deepcopy(var_values)
            new_var_values[expr[1]] = i
            res.append(interpret(expr[2], ctx, values, new_var_values))
        return res

if interpret(expr, ctx, values, var_values) == [[1, 4, 7], [2, 5, 8], [3, 6, 9]]:
    print('Success!')
    print(dex_expr)
    print(expr)" dex_expr expr) in 


  List.iter ~f:(fun expr -> 
    let (t, ctx') = typecheck expr ctx in
    if (equal_dextype t output_type) then 
      let dex_expr = expr_to_string expr in
      let py_expr = expr_to_python_tuple expr in
      (* print_string (dex_expr ^ "\n"); *)
      let _ = run_expr dex_expr py_expr in 
      ()
      (* print_ctx ctx'; *)
    else ()
  ) exprs