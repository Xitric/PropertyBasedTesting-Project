open QCheck

module ExpressionGenerator = struct

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
    ("+", Function(Integer, Function(Integer, Integer)));
    ("-", Function(Integer, Function(Integer, Integer)));
    ("*", Function(Integer, Function(Integer, Integer)));
    ("a", Integer);
    ("b", Integer)
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
  let indir_gen goal_type scope fuel =
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

    (* Step 2: Generate as many arguments as necessary to resolve the chosen generator to the goal type *)
    (* Goal: Function(Integer, Function(Integer, Integer)) *)
    (* Operator: Function(Integer, Function(Integer, Integer)) *)
    (* Result: Some(Variable("+")) *)

    (* Goal: Function(Integer, Integer) *)
    (* Operator: Function(Integer, Function(Integer, Integer)) *)
    (* Result: Some(OperatorApplication(2, Variable("+"))) *)

    (* Goal: Integer *)
    (* Operator: Function(Integer, Function(Integer, Integer)) *)
    (* Result: Some(OperatorApplication(8, OperatorApplication(2, Variable("+")))) *)

    (* Step 3: Ensure that we do not get errors with empty lists, and return the result *)


  let expression_gen goal_type scope fuel =
    oneof (List.concat [
      literal_gen goal_type;
      variable_gen goal_type scope;
      (* indir_gen goal_type scope fuel *)
    ])

end
