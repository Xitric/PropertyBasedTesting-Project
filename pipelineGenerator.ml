open QCheck
open ExpressionGenerator

(* type value_type =
  | Primitive of expression_type
  | Tuple of expression_type list *)

type execute_pipeline =
  | Reduce
  | Mean
  | Median
  | Mode
  | Var
  | StDev
  | Min
  | Max
  | Count

type pipeline_node =
  | Filter of tree_node * pipeline_node option
  (* | Abs of tree_node *)
  | Map of tree_node * string * pipeline_node option
  | Window of int * execute_pipeline * pipeline_node option

(* Scope for VarIDs *)
let variables = [
  (* E.g. *)
  (* ("a", Integer) *)
]

open Gen
(* Generator for variable identifiers *)
let identifier_gen =
  let identifier_char_gen = frequency [
    (8, char_range 'a' 'z');
    (8, char_range 'A' 'Z');
    (1, return '_');
  ] in
  let identifier_string_gen = string_size (int_bound 12) ~gen:(identifier_char_gen) in
  let rec identifier_gen_inner = function
    | "" | "_" -> identifier_string_gen >>= fun id -> identifier_gen_inner id
    | id -> return id in
  identifier_string_gen >>= fun id -> identifier_gen_inner id

(* Generator for types *)
let type_gen =
  oneofl [
    Integer; Float; Boolean; String
  ]

(* Generator for execute pipelines *)
let execute_gen = 
  oneofl [
    Reduce; Mean; Median; Mode; Var; StDev; Min; Max; Count;
  ]

(* Generator for filter *)
let rec filter_gen variables fuel =
  expression_gen Boolean (variables @ global_scope) 6 >>= function
    | None -> return None
    | Some expression -> pipeline_gen variables (fuel/2) >>= fun next ->
      return (Some (Filter(expression, next)))

(* Generator for map *)
and map_gen variables fuel =
  type_gen >>= fun expression_type ->
    expression_gen expression_type (variables @ global_scope) 6 >>= function
      | None -> return None
      | Some expression -> identifier_gen >>= fun var ->
        pipeline_gen [(var,expression_type)] (fuel/2) >>= fun next ->
          return (Some(Map(expression,var,next)))

(* Generator for window *)
and window_gen variables fuel = 
  if List.length variables > 1 then
    return None
  else
    (int_range 1 16) >>= fun number ->
      execute_gen >>= fun expip ->
        pipeline_gen variables (fuel/2) >>= fun next ->
          return (Some(Window(number, expip, next)))

(* Pipeline generator *)
and pipeline_gen variables fuel =
  if fuel = 0 then
    return None
  else
    frequency [
      (10, filter_gen variables fuel);
      (10, map_gen variables fuel);
      (1, window_gen variables fuel)
    ]

let string_of_execute = function
  | Reduce -> "reduce"
  | Mean -> "mean"
  | Median -> "median"
  | Mode -> "mode"
  | Var -> "var"
  | StDev -> "stdev"
  | Min -> "min"
  | Max -> "max"
  | Count -> "count"

let rec string_of_pipeline_node = function
  | None -> ""
  | Some pipe -> match pipe with
    | Filter (expression, next) -> ".filter[" ^ (string_of_tree_node expression) ^ "]" ^ (string_of_pipeline_node next)
    | Map (expression, id, next) -> ".map[" ^ (string_of_tree_node expression) ^ " -> " ^ id ^ "]" ^ (string_of_pipeline_node next)
    | Window (width, execute, next) -> ".byWindow[" ^ (string_of_int width) ^ "]." ^ (string_of_execute execute) ^ (string_of_pipeline_node next)
    (* | _ -> "AGSHBASIGBAS? EUUHGH! SPRFFFFF!" *)

