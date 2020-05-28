open QCheck
open Str

(* Types *)
type expression_type = 
  | Integer
  | Float
  | Boolean
  | String
  | Function of expression_type * expression_type [@@deriving show { with_path = false }]

type boxed_literal =
  | BoxedInteger of int
  | BoxedFloat of float
  | BoxedBoolean of bool
  | BoxedString of string [@@deriving show { with_path = false }]

type tree_node =
  | Literal of boxed_literal
  | Variable of string * expression_type * int
  | OperatorApplication of tree_node * expression_type * tree_node * expression_type
  | ConditionalApplication of tree_node * tree_node * tree_node * expression_type [@@deriving show { with_path = false }]

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
  ("+", Function(Boolean, Function(String, String)), 14);

  (* Subtraction *)
  ("-", Function(Float, Function(Float, Float)), 14);
  ("-", Function(Integer, Function(Integer, Integer)), 14);

  (* Multiplication *)
  ("*", Function(Float, Function(Float, Float)), 15);
  ("*", Function(Integer, Function(Integer, Integer)), 15);

  (* Division *)
  ("/", Function(Float, Function(Float, Float)), 15);
  ("/", Function(Integer, Function(Integer, Integer)), 15);

  (* Exponentiation always returns a float *)
  (* F*** this s*** in particular *)
  (* ("**", Function(Float, Function(Float, Float)), 16);
  ("**", Function(Integer, Function(Integer, Float)), 16); *)

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

let identifier_of (i, _, _) = i
let type_of (_, t, _) = t
let precedence_of (_, _, p) = p

let rec get_precedence = function
  | Literal _ -> 21
  | Variable(_, _, p) -> p
  | OperatorApplication(_, _, child, _) -> get_precedence child
  | ConditionalApplication _ -> 4

(* Used to determine the type that a tree node resolves to when applied *)
(* completely *)
let rec resolves_to = function
  | Literal lit -> (match lit with
    | BoxedInteger _ -> Integer
    | BoxedFloat _ -> Float
    | BoxedBoolean _ -> Boolean
    | BoxedString _ -> String)
  | Variable(_, t, _) ->
    let rec resolves_to_function = function
      | Function(_, next) -> resolves_to_function next
      | t -> t in
    resolves_to_function t
  | OperatorApplication(_, _, child, _) -> resolves_to child
  | ConditionalApplication(_, _, _, t) -> t

(* Determines if two types are compatible, such as using an Integer in place *)
(* of a Float *)
let rec types_compatible expected proposed =
  if expected == proposed then true else
  match (expected, proposed) with
    | (Float, Integer) -> true
    | (Function(t_expected, r_expected), Function(t_proposed, r_proposed)) ->
      types_compatible t_expected t_proposed && types_compatible r_expected r_proposed
    | _ -> false

let rec mutate_variable mutator = function
  | Variable(id, typ, precedence) ->
    mutator id typ precedence
  | OperatorApplication(a, a_type, b, b_type) ->
    let a' = mutate_variable mutator a in
    OperatorApplication(a', a_type, b, b_type)
  | ConditionalApplication(a, b, c, typ) ->
    let a' = mutate_variable mutator a in
    let b' = mutate_variable mutator b in
    let c' = mutate_variable mutator c in
    ConditionalApplication(a', b', c', typ)
  | exp -> exp

(* Generators *)
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
  (* Capture all readable characters except quotation marks, dollar sign, *)
  (* and backslash. These give problems in Groovy. *)
  (* Weighed by the number of characters in each range *)
  (2, char_range ' ' '!');
  (1, return '#');
  (55, char_range '%' '[');
  (34, char_range ']' '~');
]
let string_gen = string_size (int_bound 32) ~gen:(char_gen)

let literal_gen literal_type = match literal_type with
  | Integer ->  [map (fun i -> Some(Literal(BoxedInteger i))) (int_gen)]
  | Float -> [map (fun f -> Some(Literal(BoxedFloat f))) (float_gen)]
  | Boolean -> [map (fun b -> Some(Literal(BoxedBoolean b))) (bool_gen)]
  | String -> [map (fun s -> Some(Literal(BoxedString s))) (string_gen)]
  | _ -> [return None]

let variable_gen variable_type scope = 
  (* Choose variables from scope that have the specified variable_type *)
  (* Return a generator choosing between the variables from above *)
  match List.filter (fun variable -> types_compatible variable_type (type_of variable)) scope with
    | [] -> [return None]
    | variables -> [map (fun s -> Some(Variable (identifier_of s, type_of s, precedence_of s))) (oneofl variables)]

let rec indir_gen goal_type scope fuel =
  (* Step 1: Determine if a given type can be resolved to the goal type *)
  let rec resolves_to_goal t = match t with
    | _ when types_compatible goal_type t -> true
    | Function(a, b) -> resolves_to_goal b
    | _ -> false in
    
  (* Step 2: Choose all operators that resolve to the goal type *)
  let operators = List.filter (fun variable -> match variable with
    | (_, Function _, _) -> resolves_to_goal (type_of variable)
    | _ -> false) scope in

  (* Step 3: Generate as many arguments as necessary to resolve the chosen generator to the goal type *)
  let rec resolve_arguments_to_goal_inner previous function_type = match function_type with 
    | _ when types_compatible goal_type function_type -> return (Some previous)
    | Function(a,return_type) -> (expression_gen a scope (fuel/2) >>= function
      | Some arg -> resolve_arguments_to_goal_inner (OperatorApplication(arg, a, previous, function_type)) return_type
      | None -> return None)
    | _ -> return None in

  (* Step 4: Ensure that we do not get errors with empty lists, and return the result *)
  [match operators with
    | [] -> return None
    | operators -> oneofl operators >>= function operator -> match operator with
      | _ when types_compatible goal_type (type_of operator) -> return (Some(Variable(identifier_of operator, type_of operator, precedence_of operator)))
      | _ -> resolve_arguments_to_goal_inner (Variable(identifier_of operator, type_of operator, precedence_of operator)) (type_of operator)]

and conditional_gen goal_type scope fuel =
  [expression_gen Boolean scope (fuel / 2) >>= function
    | None -> return None
    | Some predicate -> expression_gen goal_type scope (fuel / 2) >>= function
      | None -> return None
      | Some left_exp -> expression_gen goal_type scope (fuel / 2) >>= function
        | None -> return None
        | Some right_exp -> return (Some(ConditionalApplication(predicate, left_exp, right_exp, goal_type)))]

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

(* Serialization *)
let rec string_of_boxed_literal = function
  | BoxedInteger i -> string_of_int i
  | BoxedFloat f -> string_of_float f
  | BoxedBoolean b -> string_of_bool b
  | BoxedString s -> "\"" ^ s ^ "\""

let parenthesise exp = "(" ^ exp ^ ")"

(* Serializer that takes operator precedence into account by adding parentheses
when necessary to ensure type correctness. Does not guarantee that
associativity is correct in relation to the original syntax tree - but this
does not violate type correctness either*, and it makes the resulting
expressions more challenging to parse. This is considered a good thing for
testing.
*There are no non-associative operators of equal precedence. *)
let rec string_of_tree_node = function
  | Literal l -> string_of_boxed_literal l
  | Variable(s, _, _) -> s
  | OperatorApplication(a, _, Variable(s, _, op_prec), _) ->
    " " ^ s ^ " " ^
    (if get_precedence a <= op_prec then parenthesise (string_of_tree_node a) else string_of_tree_node a)
  | OperatorApplication(l, _, (OperatorApplication(r, _, Variable(s, _, op_prec), _)), _) ->
    let left_precedence = get_precedence l in
    let right_precedence = get_precedence r in
    (if left_precedence < op_prec then parenthesise (string_of_tree_node l) else string_of_tree_node l)
    ^ " " ^ s ^ " " ^
    (if right_precedence <= op_prec then parenthesise (string_of_tree_node r) else string_of_tree_node r)
  | OperatorApplication(a, _, b, _) -> (string_of_tree_node a) ^ (string_of_tree_node b)
  | ConditionalApplication(a, b, c, _) ->
    (if get_precedence a <= 4 then
      parenthesise (string_of_tree_node a)
    else
      (string_of_tree_node a))
    ^ " ? " ^
    (string_of_tree_node b)
    ^ " : " ^
    (string_of_tree_node c)

let serialize_exp = function
  | None -> failwith "ERR"
  | Some ast -> print_endline (string_of_tree_node ast); ast

(* Shrinker *)
let (<+>) = Iter.(<+>)

let literal_shrinker = function
  | BoxedInteger old ->
    Iter.map (fun v -> Literal (BoxedInteger v)) (Shrink.int old)
  | BoxedFloat old ->
    Iter.map (fun v -> Literal (BoxedInteger v)) (Shrink.int (int_of_float old))
  | BoxedBoolean old ->
    if not old then
      Iter.return (Literal (BoxedBoolean true))
    else Iter.empty
  | BoxedString old ->
    Iter.map (fun v -> Literal (BoxedString v)) (Shrink.string old)

let make_lit = function
  | Integer -> Some (Literal (BoxedInteger (generate1 int_gen)))
  | Float -> Some (Literal (BoxedFloat (generate1 float_gen)))
  | Boolean -> Some (Literal (BoxedBoolean (generate1 bool_gen)))
  | String -> Some (Literal (BoxedString (generate1 string_gen)))
  | _ -> None

let rec tree_node_shrinker = function
  | Literal l -> literal_shrinker l
  | Variable (s, t, i) -> (match make_lit t with
    | None -> Iter.empty
    | Some lit -> Iter.return lit)
  | OperatorApplication (a, a_type, child, child_type) ->
      (match child with
        | OperatorApplication(b, b_type, var, _) ->
          let return_type = resolves_to var in
          (match make_lit return_type with
            | None -> Iter.empty
            | Some lit -> Iter.return lit)
          <+>
          (if types_compatible return_type a_type then Iter.return a else Iter.empty)
          <+>
          (if types_compatible return_type b_type then Iter.return b else Iter.empty)
        | _ -> Iter.empty)
      <+> Iter.map (fun a' -> OperatorApplication(a', a_type, child, child_type)) (tree_node_shrinker a)
      <+> (match child with
        | Variable _ -> Iter.empty
        | _ -> (Iter.map (fun child' -> OperatorApplication(a, a_type, child', child_type)) (tree_node_shrinker child))
      )
  | ConditionalApplication (a, b, c, typ) ->
    Iter.of_list [b;c]
    <+> (if (types_compatible typ Boolean) then Iter.return a else Iter.empty)
    <+> Iter.map (fun a' -> ConditionalApplication(a', b, c, typ)) (tree_node_shrinker a)
    <+> Iter.map (fun b' -> ConditionalApplication(a, b', c, typ)) (tree_node_shrinker b)
    <+> Iter.map (fun c' -> ConditionalApplication(a, b, c', typ)) (tree_node_shrinker c)
