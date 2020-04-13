open QCheck

(* Types *)
type expression_type = 
  | Integer
  | Float
  | Boolean
  | String
  | Function of expression_type * expression_type

type boxed_literal =
  | BoxedInteger of int
  | BoxedFloat of float
  | BoxedBoolean of bool
  | BoxedString of string

type tree_node =
  | Literal of boxed_literal
  | Variable of string * int
  | OperatorApplication of tree_node * tree_node
  | ConditionalApplication of tree_node * tree_node * tree_node

(* The integer represents operator precedence, and is taken from *)
(* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence#Table *)
let global_scope = [
  (* Addition *)
  ("+", Function(Float, Function(Float, Float)), 14);
  ("+", Function(Integer, Function(Integer, Integer)), 14);

  (* String concatenation *)
  ("+", Function(String, Function(String, String)), 14);
  ("+", Function(String, Function(Float, String)), 14);
  ("+", Function(Float, Function(String, String)), 14);
  ("+", Function(String, Function(Integer, String)), 14);
  ("+", Function(Integer, Function(String, String)), 14);
  ("+", Function(String, Function(Boolean, String)), 14);
  ("+", Function(Boolean, Function(String, String)), 14);

  (* Subtraction *)
  ("-", Function(Float, Function(Float, Float)), 14);
  ("-", Function(Integer, Function(Integer, Integer)), 14);

  (* Multiplication *)
  ("*", Function(Float, Function(Float, Float)), 15);
  ("*", Function(Integer, Function(Integer, Integer)), 15);

  (* String repetition - Not supported by DSL *)
  (* ("*", Function(String, Function(Integer, String)), 15);
  ("*", Function(Integer, Function(String, String)), 15); *)

  (* Division always returns a float *)
  ("/", Function(Float, Function(Float, Float)), 15);
  ("/", Function(Integer, Function(Integer, Float)), 15);

  (* Exponentiation always returns a float *)
  ("**", Function(Float, Function(Float, Float)), 16);
  ("**", Function(Integer, Function(Integer, Float)), 16);

  (* Boolean operators *)
  ("&&", Function(Boolean, Function(Boolean, Boolean)), 6);
  ("||", Function(Boolean, Function(Boolean, Boolean)), 5);
  ("!", Function(Boolean, Boolean), 17);

  (* Boolean comparisons *)
  ("==", Function(Float, Function(Float, Boolean)), 11);
  ("==", Function(Integer, Function(Integer, Boolean)), 11);
  ("==", Function(Boolean, Function(Boolean, Boolean)), 11);
  ("==", Function(String, Function(String, Boolean)), 11);

  ("!=", Function(Float, Function(Float, Boolean)), 11);
  ("!=", Function(Integer, Function(Integer, Boolean)), 11);
  ("!=", Function(Boolean, Function(Boolean, Boolean)), 11);
  ("!=", Function(String, Function(String, Boolean)), 11);

  ("<", Function(Float, Function(Float, Boolean)), 12);
  ("<", Function(Integer, Function(Integer, Boolean)), 12);
  ("<=", Function(Float, Function(Float, Boolean)), 12);
  ("<=", Function(Integer, Function(Integer, Boolean)), 12);

  (">", Function(Float, Function(Float, Boolean)), 12);
  (">", Function(Integer, Function(Integer, Boolean)), 12);
  (">=", Function(Float, Function(Float, Boolean)), 12);
  (">=", Function(Integer, Function(Integer, Boolean)), 12);
]

let rec get_precedence = function
  | Literal _ -> 21
  (* | Literal _ -> 0 *)
  | Variable(_, p) -> p
  | OperatorApplication(_, child) -> get_precedence child
  | ConditionalApplication _ -> 4

open Gen

let int_gen = frequency [
  (3, small_signed_int);
  (1, oneofl int_corners);
  (1, int)
]

let float_gen = frequency [
  (1, float);
]

let bool_gen = frequency [
  (1, bool)
]

let char_gen = frequency [
  (* Capture all readable characters except quotation marks and backslash *)
  (* Weighed by the number of characters in each range *)
  (57, char_range '#' '[');
  (34, char_range ']' '~');
  (2, char_range ' ' '!');
]
let string_gen = string_size (int_bound 32) ~gen:(char_gen)

let literal_gen literal_type = match literal_type with
  | Integer ->  [map (fun i -> Some(Literal(BoxedInteger i))) (int_gen)]
  | Float -> [map (fun f -> Some(Literal(BoxedFloat f))) (float_gen)]
  | Boolean -> [map (fun b -> Some(Literal(BoxedBoolean b))) (bool_gen)]
  | String -> [map (fun s -> Some(Literal(BoxedString s))) (string_gen)]
  | _ -> [return None]

let identifier_of (i, _, _) = i
let type_of (_, t, _) = t
let precedence_of (_, _, p) = p

let variable_gen variable_type scope = 
  (* Choose variables from scope that have the specified variable_type *)
  (* Return a generator choosing between the variables from above *)
  match List.filter (fun variable -> type_of variable = variable_type) scope with
    | [] -> [return None]
    | variables -> [map (fun s -> Some(Variable (identifier_of s, precedence_of s))) (oneofl variables)]

let rec indir_gen goal_type scope fuel =
  (* Step 1: Determine if a given type can be resolved to the goal type *)
  let rec resolves_to_goal t = match t with
    | _ when goal_type = t -> true
    | Function(a, b) -> resolves_to_goal b
    | _ -> false in
    
  (* Step 2: Choose all operators that resolve to the goal type *)
  let operators = List.filter (fun variable -> match variable with
    | (_, Function _, _) -> resolves_to_goal (type_of variable)
    | _ -> false) scope in

  (* Step 3: Generate as many arguments as necessary to resolve the chosen generator to the goal type *)
  let rec resolve_arguments_to_goal_inner previous function_type = match function_type with 
    | _ when function_type = goal_type -> return (Some previous)
    | Function(a,return_type) -> (expression_gen a scope (fuel/2) >>= function
      | Some arg -> resolve_arguments_to_goal_inner (OperatorApplication(arg, previous)) return_type
      | None -> return None)
    | _ -> return None in

  (* Step 3: Ensure that we do not get errors with empty lists, and return the result *)
  [match operators with
    | [] -> return None
    | operators -> oneofl operators >>= function operator -> match operator with
      | _ when (type_of operator) = goal_type -> return (Some(Variable(identifier_of operator, precedence_of operator)))
      | _ -> resolve_arguments_to_goal_inner (Variable(identifier_of operator, precedence_of operator)) (type_of operator)]

and conditional_gen goal_type scope fuel =
  [expression_gen Boolean scope (fuel / 2) >>= function
    | None -> return None
    | Some predicate -> expression_gen goal_type scope (fuel / 2) >>= function
      | None -> return None
      | Some left_exp -> expression_gen goal_type scope (fuel / 2) >>= function
        | None -> return None
        | Some right_exp -> return (Some(ConditionalApplication(predicate, left_exp, right_exp)))]

and expression_gen goal_type scope fuel =
  let generators = if fuel = 0 then
    List.concat [
      List.map (fun g -> (1, g)) (literal_gen goal_type);
      List.map (fun g -> (1, g)) (variable_gen goal_type scope)
    ]
  else
    List.concat [
      List.map (fun g -> (1, g)) (literal_gen goal_type);
      List.map (fun g -> (1, g)) (variable_gen goal_type scope);
      List.map (fun g -> (10, g)) (indir_gen goal_type scope fuel);
      List.map (fun g -> (6, g)) (conditional_gen goal_type scope fuel)
    ] in
  
  match generators with
    | [] -> return None
    | generators -> frequency generators >>= function
      | None -> expression_gen goal_type scope fuel
      | Some value -> return (Some value)

let rec string_of_boxed_literal = function
  | BoxedInteger i -> string_of_int i
  | BoxedFloat f -> string_of_float f
  | BoxedBoolean b -> string_of_bool b
  | BoxedString s -> "\"" ^ s ^ "\""

(* let rec string_of_tree_node = function
  | Literal l -> string_of_boxed_literal l
  | Variable(s, _) -> s
  | OperatorApplication(a, Variable(s, p)) -> " " ^ s ^ " " ^ (string_of_tree_node a)
  (* | OperatorApplication((OperatorApplication(_, Variable _) as l), (OperatorApplication _ as r)) -> *)
  | OperatorApplication((OperatorApplication(_, Variable _) as l), (OperatorApplication(_, Variable _) as r)) ->
  (* | OperatorApplication((OperatorApplication _ as l), (OperatorApplication _ as r)) -> *)
    let left_precedence = get_precedence l in
    let right_precedence = get_precedence r in
    if left_precedence > right_precedence then
      (string_of_tree_node l) ^ "(" ^ (string_of_tree_node r) ^ ")"
    else if left_precedence < right_precedence then
      "(" ^ (string_of_tree_node l) ^ ")" ^ (string_of_tree_node r)
    else
      (string_of_tree_node l) ^ (string_of_tree_node r)
  | OperatorApplication(a, b) -> (string_of_tree_node a) ^ (string_of_tree_node b)
  | ConditionalApplication(a, b, c) -> "(" ^ (string_of_tree_node a) ^ " ? " ^ (string_of_tree_node b) ^ " : " ^ (string_of_tree_node c) ^ ")" *)

let parenthesise exp = "(" ^ exp ^ ")"

let rec string_of_tree_node = function
  | Literal l -> string_of_boxed_literal l
  | Variable(s, _) -> s
  | OperatorApplication(a, Variable(s, p)) -> " " ^ s ^ " " ^ (string_of_tree_node a)
  (* | OperatorApplication((OperatorApplication(_, Variable _) as l), (OperatorApplication _ as r)) -> *)
  (* | OperatorApplication((OperatorApplication(_, Variable _) as l), (OperatorApplication(_, Variable _) as r)) -> *)
  (* | OperatorApplication((OperatorApplication _ as l), (OperatorApplication _ as r)) -> *)
  (* | OperatorApplication((OperatorApplication _ as l), (OperatorApplication(r, Variable(_, op_prec)))) -> *)
  | OperatorApplication(l, (OperatorApplication(r, Variable(s, op_prec)))) ->
    let left_precedence = get_precedence l in
    let right_precedence = get_precedence r in
    if left_precedence < op_prec then parenthesise (string_of_tree_node l) else string_of_tree_node l
    ^
    s
    ^
    if right_precedence < op_prec then parenthesise (string_of_tree_node r) else string_of_tree_node r


    (* if left_precedence < op_prec then
      (string_of_tree_node l) ^ "(" ^ (string_of_tree_node r) ^ ")"
    else if left_precedence < right_precedence then
      "(" ^ (string_of_tree_node l) ^ ")" ^ (string_of_tree_node r)
    else
      (string_of_tree_node l) ^ (string_of_tree_node r) *)
  | OperatorApplication(a, b) -> (string_of_tree_node a) ^ (string_of_tree_node b)
  | ConditionalApplication(a, b, c) -> (string_of_tree_node a) ^ " ? " ^ (string_of_tree_node b) ^ " : " ^ (string_of_tree_node c)

(* let rec broken_string_of_tree_node = function
  | Literal l -> string_of_boxed_literal l
  | Variable(s, _) -> s
  | OperatorApplication(a, Variable(s, p)) -> " " ^ s ^ " " ^ (string_of_tree_node a)
  (* | OperatorApplication((OperatorApplication(_, Variable _) as l), (OperatorApplication _ as r)) -> *)
  | OperatorApplication((OperatorApplication _ as l), (OperatorApplication _ as r)) ->
    let left_precedence = get_precedence l in
    let right_precedence = get_precedence r in
    if left_precedence > right_precedence then
      (string_of_tree_node l) ^ "(" ^ (string_of_tree_node r) ^ ")"
    else if left_precedence < right_precedence then
      "(" ^ (string_of_tree_node l) ^ ")" ^ (string_of_tree_node r)
    else
      (string_of_tree_node l) ^ (string_of_tree_node r)
  | OperatorApplication(a, b) -> (string_of_tree_node a) ^ (string_of_tree_node b)
  | ConditionalApplication(a, b, c) -> "(" ^ (string_of_tree_node a) ^ " ? " ^ (string_of_tree_node b) ^ " : " ^ (string_of_tree_node c) ^ ")" *)

let serialize_exp = function
  | None -> failwith "ERR"
  (* | Some ast -> print_endline (string_of_tree_node ast ^ "\n" ^ broken_string_of_tree_node ast); ast *)
  | Some ast -> print_endline (string_of_tree_node ast); ast


(* (5+2) * 9 *)
(* 2 * (9+5) *)
