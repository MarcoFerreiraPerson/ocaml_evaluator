open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Print env to stdout *)
let print_env_std (env : environment): unit=
 List.fold_left (fun _ (var, value) ->
      match value with
        | Int_Val i -> Printf.printf "- %s => %s\n" var (string_of_int i)
        | Bool_Val b -> Printf.printf "- %s => %s\n" var (string_of_bool b)
        | Closure _ -> ()) () (prune_env env)

(* Print env to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      match value with
        | Int_Val i -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_int i))
        | Bool_Val b -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_bool b))
        | Closure _ -> acc
      ) "" (prune_env env)


(***********************)
(****** Your Code ******)
(***********************)

(* evaluate an arithmetic expression in an environment *)
let rec eval_expr (e : exp) (env : environment) : value =
  match e with
  | Number n -> Int_Val n
  | True -> Bool_Val true
  | False -> Bool_Val false
  | Var x -> 
      (match List.assoc_opt x env with
       | Some v -> v
       | None -> raise UndefinedVar)
  | Plus (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val n1, Int_Val n2 -> Int_Val (n1 + n2)
       | _, _ -> raise TypeError)
  | Minus (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val n1, Int_Val n2 -> Int_Val (n1 - n2)
       | _, _ -> raise TypeError)
  | Times (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val n1, Int_Val n2 -> Int_Val (n1 * n2)
       | _, _ -> raise TypeError)
  | Div (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val n1, Int_Val n2 -> 
           if n2 = 0 then raise DivByZeroError
           else Int_Val (n1 / n2)
       | _, _ -> raise TypeError)
  | Mod (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val n1, Int_Val n2 -> 
           if n2 = 0 then raise DivByZeroError
           else Int_Val (n1 mod n2)
       | _, _ -> raise TypeError)
  | And (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Bool_Val b1, Bool_Val b2 -> Bool_Val (b1 && b2)
       | _, _ -> raise TypeError)
  | Or (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Bool_Val b1, Bool_Val b2 -> Bool_Val (b1 || b2)
       | _, _ -> raise TypeError)
  | Not e1 ->
      (match eval_expr e1 env with
       | Bool_Val b -> Bool_Val (not b)
       | _ -> raise TypeError)
  | Lt (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val n1, Int_Val n2 -> Bool_Val (n1 < n2)
       | _, _ -> raise TypeError)
  | Leq (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val n1, Int_Val n2 -> Bool_Val (n1 <= n2)
       | _, _ -> raise TypeError)
  | Eq (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val n1, Int_Val n2 -> Bool_Val (n1 = n2)
       | Bool_Val b1, Bool_Val b2 -> Bool_Val (b1 = b2)
       | _, _ -> raise TypeError)
  | Fun (x, body) -> Closure (env, x, body)
  | App (e1, e2) ->
      (match eval_expr e1 env with
       | Closure (closure_env, x, body) ->
           let arg_val = eval_expr e2 env in
           let new_env = (x, arg_val) :: closure_env in
           eval_expr body new_env
       | _ -> raise TypeError)
  
(* evaluate a command in an environment *)
let rec eval_command (c : com) (env : environment) : environment =
  match c with
  | Skip -> env
  | Comp (c1, c2) ->
      let _env = eval_command c1 env in
      eval_command c2 _env
  | Declare (dtype, var) ->
      let default_value = match dtype with
        | Int_Type -> Int_Val 0
        | Bool_Type -> Bool_Val false
        | Lambda_Type -> Closure (env, "x", Var "x")
      in
      (var, default_value) :: env
  | Assg (var, expr) ->
      let value = eval_expr expr env in
      let _ = 
        match List.assoc_opt var env with
        | None -> raise UndefinedVar
        | Some existing_val ->
            match existing_val, value with
            | Int_Val _, Int_Val _ -> ()
            | Bool_Val _, Bool_Val _ -> ()
            | Closure _, Closure _ -> ()
            | _, _ -> raise TypeError
      in
      (var, value) :: env
  | Cond (cond, if_branch, else_branch) ->
      (match eval_expr cond env with
       | Bool_Val true -> eval_command if_branch env
       | Bool_Val false -> eval_command else_branch env
       | _ -> raise TypeError)
  | While (cond, body) ->
      (match eval_expr cond env with
       | Bool_Val true ->
           let _env = eval_command body env in
           eval_command (While (cond, body)) _env
       | Bool_Val false -> env
       | _ -> raise TypeError)
  | For (count_expr, body) ->
      (match eval_expr count_expr env with
       | Int_Val n ->
           let rec loop n env =
             if n <= 0 then env
             else let _env = eval_command body env in
                  loop (n-1) _env
           in
           loop n env
       | _ -> raise TypeError)
