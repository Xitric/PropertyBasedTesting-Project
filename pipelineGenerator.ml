open QCheck
open ExpressionGenerator

type execute_pipeline =
  | Mean
  | Median
  | Var
  | StDev
  | Min
  | Max
  | Count 

type pipeline_node =
  | Filter of tree_node * pipeline_node option
  | Map of tree_node * string * pipeline_node option
  | Window of int * execute_pipeline * pipeline_node option

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
    Mean; Median; Var; StDev; Min; Max;
  ]

(* Generator for filter *)
let rec filter_gen goal_type current_type variables fuel =
  expression_gen Boolean (variables @ global_scope) 6 >>= function
    | None -> return None
    | Some expression -> pipeline_gen_internal goal_type current_type variables (fuel/2) >>= fun next ->
      return (Some (Filter(expression, next)))

(* Generator for map *)
and map_gen goal_type variables fuel =
  type_gen >>= fun expression_type ->
    map_gen_internal goal_type expression_type variables fuel

and map_gen_internal goal_type expression_type variables fuel =
  expression_gen expression_type (variables @ global_scope) 6 >>= function
    | None -> return None
    | Some expression -> identifier_gen >>= fun var ->
      pipeline_gen_internal goal_type expression_type [(var, expression_type, 21)] (fuel/2) >>= fun next ->
        return (Some(Map(expression,var,next)))

(* Generator for window *)
and window_gen goal_type variables fuel = 
  if List.length variables > 1 then
    return None
  else 
    (int_range 1 16) >>= fun number ->
      execute_gen >>= fun expip ->
        pipeline_gen_internal goal_type Integer variables (fuel/2) >>= fun next ->
          return (Some(Window(number, expip, next)))

and pipeline_gen_internal goal_type current_type variables fuel =
  if fuel = 0 then
    if goal_type <> current_type then
      (* The pipeline ends with the wrong type, so we force a map at the end *)
      map_gen_internal goal_type goal_type variables fuel
    else
      return None
  else
    frequency [
      (10, filter_gen goal_type current_type variables fuel);
      (10, map_gen goal_type variables fuel);
      (1, window_gen goal_type variables fuel)
    ]

(* Pipeline generator *)
and pipeline_gen goal_type variables fuel =
  if fuel = 0 then
    (* TODO: Invoke generator with specific target type, similarly to above TODO *)
    return None
  else
    frequency [
      (10, filter_gen goal_type Integer variables fuel);
      (10, map_gen goal_type variables fuel);
      (1, window_gen goal_type variables fuel)
    ]
 
let string_of_execute = function
  | Mean -> "mean"
  | Median -> "median"
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

(* Shrinker *)
let (<+>) = Iter.(<+>)
let rec pipeline_shrinker = function
  | Filter(exp, next) ->
    (match next with
      | None -> Iter.empty
      | Some n ->
        Iter.return n
        <+> Iter.map (fun next' -> Filter(exp, Some next')) (pipeline_shrinker n))
    <+> Iter.map (fun exp' -> Filter(exp', next)) (tree_node_shrinker exp)
  | Map(exp, id, next) ->
    (match next with
      | None -> Iter.empty
      | Some n ->
        Iter.map (fun next' -> Map(exp, id, Some next')) (pipeline_shrinker n))
    <+> Iter.map (fun exp' -> Map(exp', id, next)) (tree_node_shrinker exp)
  | Window(width, exec, next) ->
    (match next with
      | None -> Iter.empty
      | Some n ->
        Iter.map (fun next' -> Window(width, exec, Some next')) (pipeline_shrinker n))
    <+> Iter.map (fun w -> Window(w, exec, next)) (Shrink.int width)
