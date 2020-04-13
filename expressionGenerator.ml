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
  | Variable of string
  | OperatorApplication of tree_node * tree_node
  | ConditionalApplication of tree_node * tree_node * tree_node

let global_scope = [
  (* Addition *)
  ("+", Function(Float, Function(Float, Float)));
  ("+", Function(Integer, Function(Integer, Integer)));

  (* String concatenation *)
  ("+", Function(String, Function(String, String)));
  ("+", Function(String, Function(Float, String)));
  ("+", Function(Float, Function(String, String)));
  ("+", Function(String, Function(Integer, String)));
  ("+", Function(Integer, Function(String, String)));
  ("+", Function(String, Function(Boolean, String)));
  ("+", Function(Boolean, Function(String, String)));

  (* Subtraction *)
  ("-", Function(Float, Function(Float, Float)));
  ("-", Function(Integer, Function(Integer, Integer)));

  (* Multiplication *)
  ("*", Function(Float, Function(Float, Float)));
  ("*", Function(Integer, Function(Integer, Integer)));

  (* String repetition *)
  ("*", Function(String, Function(Integer, String)));
  ("*", Function(Integer, Function(String, String)));

  (* Division always returns a float *)
  ("/", Function(Float, Function(Float, Float)));
  ("/", Function(Integer, Function(Integer, Float)));

  (* Exponentiation always returns a float *)
  ("**", Function(Float, Function(Float, Float)));
  ("**", Function(Integer, Function(Integer, Float)));

  (* Boolean operators *)
  ("&&", Function(Boolean, Function(Boolean, Boolean)));
  ("||", Function(Boolean, Function(Boolean, Boolean)));
  ("!", Function(Boolean, Boolean));

  (* Boolean comparisons *)
  ("==", Function(Float, Function(Float, Boolean)));
  ("==", Function(Integer, Function(Integer, Boolean)));
  ("==", Function(Boolean, Function(Boolean, Boolean)));
  ("==", Function(String, Function(String, Boolean)));

  ("!=", Function(Float, Function(Float, Boolean)));
  ("!=", Function(Integer, Function(Integer, Boolean)));
  ("!=", Function(Boolean, Function(Boolean, Boolean)));
  ("!=", Function(String, Function(String, Boolean)));

  ("<", Function(Float, Function(Float, Boolean)));
  ("<", Function(Integer, Function(Integer, Boolean)));
  ("<=", Function(Float, Function(Float, Boolean)));
  ("<=", Function(Integer, Function(Integer, Boolean)));

  (">", Function(Float, Function(Float, Boolean)));
  (">", Function(Integer, Function(Integer, Boolean)));
  (">=", Function(Float, Function(Float, Boolean)));
  (">=", Function(Integer, Function(Integer, Boolean)));
]

(* option QCheck.Gen.t list *)
(* Literals, operator applications (2+5, 7/3), variables (+, /, a, b, c), conditional operator applications, ... *)
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

(* variable_type: Integer, Function(Integer, Integer) *)
(* scope: [
  ("+", Function(Integer, Function(Integer, Integer)));
  ("-", Function(Integer, Function(Integer, Integer)));
  ("*", Function(Integer, Function(Integer, Integer)));
  ("a", Integer);
  ("b", Integer)
] *)
let type_of var = snd var
let identifier_of var = fst var
(* tree_node option QCheck.Gen.t list *)
let variable_gen variable_type scope = 
  (* Choose variables from scope that have the specified variable_type *)

  (* ("a", Integer); *)
  (* Variable("a") *)
  (* Return a generator choosing between the variables from above *)
  match List.filter (fun variable -> type_of variable = variable_type) scope with
    | [] -> [return None]
    | variables -> [map (fun s -> Some(Variable (fst s))) (oneofl variables)]

(* scope: [
  ("+", Function(Integer, Function(Integer, Integer)));
  ("-", Function(Integer, Function(Integer, Integer)));
  ("*", Function(Integer, Function(Integer, Integer)));
  ("a", Integer);
  ("b", String)
] *)
(* Function(Integer, Function(String, Integer)) *)
(* Function(Integer, Function(Integer, String)) *)  
let rec indir_gen goal_type scope fuel =
  (* Step 0.5: Determine if a given type can be resolved to the goal type *)

  (* Goal: Function(Integer, Integer) *)
  (* Iter 1: t = Function(Integer, Function(Integer, Integer)) *)
  (* Iter 2 (Base case): t = Function(Integer, Integer) *)
  let rec resolves_to_goal t = match t with
    | _ when goal_type = t -> true
    | Function(a, b) -> resolves_to_goal b
    | _ -> false in
    
  (* Step 1: Choose all operators that resolve to the goal type *)
  let operators = List.filter (fun variable -> match variable with
    | (_, Function _) -> resolves_to_goal (type_of variable)
    | _ -> false) scope in
  (* [
    ("+", Function(Integer, Function(Integer, Integer))),
    ("-", Function(Integer, Integer)),
    ...
  ] *)

  (* Step 2: Generate as many arguments as necessary to resolve the chosen generator to the goal type *)
  
  (* Goal:      Function(Integer, Function(Integer, Integer)) *)
  (* Operator:  Function(Integer, Function(Integer, Integer)) *)
  (* Result:    Some(Variable("+")) *)
  let open Gen in

  
  (* type tree_node =
  | Literal of boxed_literal
  | Variable of string
  | OperatorApplication of tree_node * tree_node
  | ConditionalApplication of tree_node * tree_node * tree_node *)

  (* Operator: Function(Integer, Function(Integer, Integer)) *)
  (* Base case: Variable("+") *)
  (* Step 1: previous=Variable("+") function_type=Function(Integer, Function(Integer, Integer)) *)
  (*    OperatorApplication(5, Variable("+")) *)
  (* Step 2: previous=OperatorApplication(5, Variable("+")) function_type=Function(Integer, Integer) *)
  (*    OperatorApplication(8, OperatorApplication(5, Variable("+"))) *)
  (* Done, function_type = Integer = goal_type *)
  let rec resolve_arguments_to_goal_inner previous function_type = match function_type with 
    | _ when function_type = goal_type -> return (Some previous)
    | Function(a,return_type) -> (expression_gen a scope (fuel/2) >>= function arg -> match arg with
      | Some a -> resolve_arguments_to_goal_inner (OperatorApplication(a, previous)) return_type
      | None -> return None)
    | _ -> return None in

  (* Step 3: Ensure that we do not get errors with empty lists, and return the result *)
  [match operators with
    | [] -> return None
    | operators -> oneofl operators >>= function operator -> match operator with
      | _ when (type_of operator) = goal_type -> return (Some(Variable(identifier_of operator)))
      | _ -> resolve_arguments_to_goal_inner (Variable(identifier_of operator)) (type_of operator)]

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

let rec string_of_tree_node = function
  | Literal l -> string_of_boxed_literal l
  | Variable s -> s
  | OperatorApplication(a, Variable s) -> " " ^ s ^ " " ^ (string_of_tree_node a)
  | OperatorApplication(a, b) -> (string_of_tree_node a) ^ (string_of_tree_node b)
  | ConditionalApplication(a, b, c) -> "(" ^ (string_of_tree_node a) ^ " ? " ^ (string_of_tree_node b) ^ " : " ^ (string_of_tree_node c) ^ ")"

let serialize_exp = function
  | None -> "None"
  | Some ast -> string_of_tree_node ast
