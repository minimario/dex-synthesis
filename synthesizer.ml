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
  if equal_dextype restype Error then (Error, Map.empty (module String)) else (restype, ctx')
| Sum e -> 
  let (t, ctx') = typecheck e ctx in
  let restype = match t with 
  | ArrowType (_, t2) -> t2
  | VarType _ | Error -> Error
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

let sample_expr = For ("k", For ("i", For ("j", Dot (Dot (Dot (Var "xs", "j"), "k"), "i"))))
let input_type = ArrowType (VarType "n", ArrowType (VarType "m", ArrowType (VarType "p", VarType "v")))
let output_type = ArrowType (VarType "m", ArrowType (VarType "n", ArrowType (VarType "p", VarType "v")))
let output_type_2 = ArrowType (VarType "m", ArrowType (VarType "n", ArrowType (VarType "p", VarType "v")))

let _ =  print_string (string_of_bool (equal_dextype output_type output_type_2))

(* let sample_expr = (Sum (For ("i", Dot (Var "xs", "i")))) *)
(* let (t, ctx) = typecheck sample_expr (String.Map.singleton "xs" input_type) *)

let depth = 10
let exprs = List.init ~f:(fun i -> gen_exprs (i+1) []) (depth-1) |> List.concat
(* let exprs = gen_exprs 1 [] *)

let expr_types = List.map ~f:(fun expr -> typecheck expr (String.Map.singleton "xs" output_type)) exprs
let _ = 
  List.iter ~f:(fun expr -> 
    let (t, _) = typecheck expr (String.Map.singleton "xs" input_type) in

    (* let _ = print_string ((expr_to_string expr) ^ "\n") in *)
    (* let _ = print_string (type_to_string output_type ^ "\n") in *)
    (* let _ = print_string (type_to_string t ^ "\n") in *)
    (* let _ = print_string (string_of_bool (String.equal (type_to_string t) (type_to_string output_type)) ^ "\n") in *)
    (* let _ = print_string (string_of_bool (equal_dextype t output_type)) in *)

    if (equal_dextype t output_type) then 
      (print_string (expr_to_string expr ^ " " ^ type_to_string t ^ "\n")) 
    else ();
  ) exprs

(* let _ = 
  print_string ((expr_to_string sample_expr) ^ "\n");
  print_string ((type_to_string t) ^ "\n");
  print_ctx ctx *)

(* let _ = print_string ((expr_to_string sample_expr) ^ "\n") *)
(* let _ = print_string ((type_to_string sample_type) ^ "\n") *)