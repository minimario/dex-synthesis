open Core;;

type var = string

type dextype = 
| VarType of var
| ArrowType of dextype * dextype
| Error

type expr = 
| Var of var
| Dot of expr * var
| Sum of expr
| For of var * expr
let rec expr_to_string = function 
  | Var v -> v
  | Dot (e, v) -> (expr_to_string e) ^ "." ^ v
  | Sum expr -> "sum (" ^ (expr_to_string expr) ^ ")"
  | For (v, e) -> ("for " ^ v ^ ". " ^ (expr_to_string e))

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
  if phys_equal restype Error then (Error, Map.empty (module String)) else (restype, ctx')
| Sum e -> 
  let (t, ctx') = typecheck e ctx in
  let restype = match t with 
  | ArrowType (_, t2) -> t2
  | VarType _ | Error -> Error
  in
  if phys_equal restype Error then (Error, Map.empty (module String)) else (restype, ctx')

let rec gen_exprs depth indices = 
  if depth = 1 then if List.length indices = 0 then [Var "xs"] else []
  else(
    
    let prev_exprs = gen_exprs (depth - 1) indices in
    let sum_exprs = List.map ~f:(fun expr -> Sum expr) prev_exprs in

    let dot_exprs = 
      List.map ~f:(fun ivar -> 
        List.map ~f:(fun expr -> Dot (expr, ivar))
          (gen_exprs (depth - 1) (List.filter ~f:(fun x -> not (phys_equal x ivar)) indices))
      ) indices |> List.concat in
    (* let dot_exprs = List.map ~f:(fun expr -> List.map ~f:(fun ivar -> Dot (expr, ivar)) indices) prev_exprs |> List.concat in *)
    let varname = "x" ^ string_of_int depth in

    let prev_exprs_with_var = gen_exprs (depth - 1) (varname::indices) in
    let for_exprs = List.map ~f:(fun expr -> For (varname, expr)) prev_exprs_with_var in

    let all_exprs = List.concat [sum_exprs; dot_exprs; for_exprs] in

    all_exprs
  )

let exprs = gen_exprs 10 []


let _ = List.iter ~f:(fun expr -> (print_string ((expr_to_string expr) ^ "\n"))) exprs
(* let sample_expr = For ("k", For ("i", For ("j", Dot (Dot (Dot (Var "xs", "j"), "k"), "i"))))
let sample_type = ArrowType (VarType "n", ArrowType (VarType "m", ArrowType (VarType "p", VarType "v"))) *)
(* let sample_expr = (Sum (For ("i", Dot (Var "xs", "i")))) *)
(* let (t, ctx) = typecheck sample_expr (String.Map.singleton "xs" sample_type)
let _ = 
  print_string ((expr_to_string sample_expr) ^ "\n");
  print_string ((type_to_string t) ^ "\n");
  print_ctx ctx *)

(* let _ = print_string ((expr_to_string sample_expr) ^ "\n") *)
(* let _ = print_string ((type_to_string sample_type) ^ "\n") *)