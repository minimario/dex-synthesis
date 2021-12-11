open Core;;

type var = string

type dextype = 
| VarType of var
| ArrowType of dextype * dextype
| Error

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

let rec expr_to_string = function 
  | Var v -> v
  | Dot (e, v) -> (expr_to_string e) ^ "." ^ v
  | Sum expr -> "sum (" ^ (expr_to_string expr) ^ ")"
  | For (v, e) -> "(for " ^ v ^ "." ^ (expr_to_string e) ^ ")"

let rec type_to_string = function
| VarType v -> v
| ArrowType (t1, t2) -> (type_to_string t1) ^ " => " ^ (type_to_string t2)
| Error -> "Error" 

let print_ctx ctx = Map.iteri ctx (fun ~key:v ~data:t -> (print_string (v ^ " : " ^ (type_to_string t) ^ "\n")))

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

let rec gen_exprs depth indices = 
  if depth = 1 then if List.length indices = 0 then [Var "xs"] else []
  else(
    
    let prev_exprs = gen_exprs (depth - 1) indices in
    let sum_exprs = List.map ~f:(fun expr -> Sum expr) prev_exprs in

    let dot_exprs = 
      List.map ~f:(fun ivar -> 
        List.map ~f:(fun expr -> Dot (expr, ivar))
          (gen_exprs (depth - 1) (List.filter ~f:(fun x -> not (equal_string x ivar)) indices))
      ) indices |> List.concat in
    (* let dot_exprs = List.map ~f:(fun expr -> List.map ~f:(fun ivar -> Dot (expr, ivar)) indices) prev_exprs |> List.concat in *)
    let varname = "x" ^ string_of_int depth in

    let prev_exprs_with_var = gen_exprs (depth - 1) (varname::indices) in
    let for_exprs = List.map ~f:(fun expr -> For (varname, expr)) prev_exprs_with_var in

    let all_exprs = List.concat [sum_exprs; dot_exprs; for_exprs] in

    all_exprs
  )

(* let sample_expr' = For ("x9", For ("x8", For ("x7", For ("x4", Dot (Dot (Dot (Dot (Var "xs", "x4"), "x9"), "x8"), "x7"))))) *)
(* let sample_expr = For ("k", For ("i", For ("j", Dot (Dot (Dot (Var "xs", "j"), "k"), "i")))) *)

(* transpose *)
let input_type_1 = ArrowType (VarType "m", ArrowType (VarType "n", VarType "v"))
let output_type_1 = ArrowType (VarType "n", ArrowType (VarType "m", VarType "v"))
let ctx_1 = String.Map.singleton "xs" input_type_1


let input_type_2 = ArrowType (VarType "n", ArrowType (VarType "m", ArrowType (VarType "p", VarType "v")))
let output_type_2 = ArrowType (VarType "m", ArrowType (VarType "n", ArrowType (VarType "p", VarType "v")))
let ctx_2 = String.Map.singleton "xs" input_type_2

let input_type_3 = ArrowType (VarType "n", ArrowType (VarType "m", VarType "v"))
let output_type_3 = VarType "v"
let ctx_3 = String.Map.singleton "xs" input_type_1

let depth = 10
let exprs = List.init ~f:(fun i -> gen_exprs (i+1) []) (depth-1) |> List.concat
let _ = 
  List.iter ~f:(fun expr -> 
    let (t, _) = typecheck expr ctx_1 in
    if (equal_dextype t output_type_1) then 
      (print_string (expr_to_string expr ^ " " ^ type_to_string t ^ "\n")) 
    else ();
  ) exprs

