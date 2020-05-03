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
and window_gen goal_type current_type variables fuel =
  (* We can only perform window operations in number types *)
  match current_type with
    | Integer
    | Float ->
      if List.length variables > 1 then
        return None
      else
        (int_range 1 16) >>= fun number ->
          execute_gen >>= fun expip ->
            pipeline_gen_internal goal_type Float variables (fuel/2) >>= fun next ->
              return (Some(Window(number, expip, next)))
    | _ -> return None

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
      (1, window_gen goal_type current_type variables fuel)
    ]

(* Pipeline generator *)
and pipeline_gen goal_type variables fuel =
  if fuel = 0 then
    return None
  else
    frequency [
      (* Pipelines always start with integers as input *)
      (10, filter_gen goal_type Integer variables fuel);
      (10, map_gen goal_type variables fuel);
      (1, window_gen goal_type Integer variables fuel)
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
let next = function
  | Some current -> (match current with
    | Filter(_, next)
    | Map(_, _, next)
    | Window(_, _, next) -> next)
  | _ -> None

let is_last_map map =
  let rec is_last = function
    | Some(Filter(_, next))
    | Some(Window(_, _, next)) -> is_last next
    | Some(Map _) -> false
    | None -> true in
  is_last (next (Some map))

let rec remove current to_remove =
  if current = to_remove then
    next (Some current)
  else
    match current with
      | Filter(exp, Some next) -> Some (Filter(exp, remove next to_remove))
      | Map(exp, id, Some next) -> Some (Map(exp, id, remove next to_remove))
      | Window(width, exec, Some next) -> Some (Window(width, exec, remove next to_remove))
      | _ -> None

let mutate_pipeline_variables mutator pipeline =
  let rec mutate_inner mutator = function
    | Filter(exp, Some next) ->
      let exp' = mutator exp in
      let next' = mutate_inner mutator next in
      Filter(exp', Some next')
    | Map(exp, id, next) ->
      let exp' = mutator exp in
      Map(exp', id, next)
    | Window(width, exec, Some next) ->
      let next' = mutate_inner mutator next in
      Window(width, exec, Some next')
    | pipeline -> pipeline in
  match pipeline with
    | Filter(exp, Some next) -> Filter(exp, Some (mutate_inner mutator next))
    | Map(exp, id, Some next) -> Map(exp, id, Some (mutate_inner mutator next))
    | Window(width, exec, Some next) -> Window(width, exec, Some (mutate_inner mutator next))
    | _ -> pipeline

(* TODO: Ability to remove windows depending on types in pipeline *)
let (<+>) = Iter.(<+>)
let rec pipeline_shrinker scope current =
  (* Shrinking recursively *)
  (match current with
    | Filter(exp, Some next) -> Iter.map (fun next' -> Filter(exp, Some next')) (pipeline_shrinker scope next)
    | Map(exp, id, Some next) -> Iter.map (fun next' -> Map(exp, id, Some next')) (pipeline_shrinker [(id, resolves_to exp)] next)
    | Window(width, exec, Some next) -> Iter.map (fun next' -> Window(width, exec, Some next')) (pipeline_shrinker scope next)
    | _ -> Iter.empty)
  <+>

  (* Removing filters *)
  (match current with
    | Filter(_, Some next) -> Iter.return next
    | Map(_, _, Some (Filter _ as filter))
    | Window(_, _, Some (Filter _ as filter)) -> (match remove current filter with
      | None -> Iter.empty
      | Some filter_removed -> Iter.return filter_removed)
    | _  -> Iter.empty)
  <+>

  (* Removing maps *)
  (match current with
    | Filter(_, Some (Map(exp, id, _) as map))
    | Map(_, _, Some (Map(exp, id, _) as map))
    | Window(_, _, Some (Map(exp, id, _) as map))
    | (Map(exp, id, _) as map) ->
      if is_last_map map then
        (* Can remove if it is the last map and it does not change the type *)
        (* from the previous scope *)
        if List.length scope = 1 && types_compatible (snd (List.hd scope)) (resolves_to exp) then
          (match remove current map with
            | None -> Iter.empty
            | Some map_removed -> Iter.return map_removed)
        else Iter.empty
      else
        (* We can always remove a map if it is followed by another one *)
        (match remove current map with
          | None -> Iter.empty
          | Some map_removed ->
            (* If the previous scope uses the same type of variable, we can just *)
            (* rename variables after removing the map *)
            if types_compatible (snd (List.hd scope)) (resolves_to exp) then (
              let new_identifier = fst (List.hd scope) in
              let variable_renamer old_id typ precedence =
                if old_id = id then
                  Variable(new_identifier, typ, precedence)
                else
                  Variable(old_id, typ, precedence)
                in
              Iter.return (mutate_pipeline_variables (mutate_variable variable_renamer) map_removed)

            (* If the previous scope uses a different type of variable, we can *)
            (* instead replace variables with literals *)
            ) else (
              let literal_replacer old_id typ precedence =
                if old_id = id then
                  match make_lit typ with
                    | Some lit -> lit
                    | None -> Variable(old_id, typ, precedence)
                else
                  Variable(old_id, typ, precedence)
                in
              Iter.return (mutate_pipeline_variables (mutate_variable literal_replacer) map_removed)
            )
        )
    | _ -> Iter.empty)
    <+>

  (* Removing windows *)
  (match current with
    | Filter(_, Some (Window _ as window)) 
    | Map(_, _, Some (Window _ as window)) -> (match remove current window with
      | None -> Iter.empty
      | Some window_removed -> Iter.return window_removed)
    | Window(_, _, Some next) -> Iter.return next
    | _  -> Iter.empty)
  <+>

  (* Shrinking expressions *)
  (match current with
    | Filter(exp, next) -> Iter.map (fun exp' -> Filter(exp', next)) (tree_node_shrinker exp)
    | Map(exp, id, next) -> Iter.map (fun exp' -> Map(exp', id, next)) (tree_node_shrinker exp)
    | Window(width, exec, next) -> Iter.map (fun width' -> Window(width', exec, next)) (Shrink.int width))


(* 390736775 *)
(* out jkvqx du_EzXXc.map["" + (true ? 0 : -91449.8009972) -> BOTAen].byWindow[0].median.map[0 -> RBYyk_].map[0 -> TFaRFNFOYxgT] *)
(* out jkvqx du_EzXXc.                                                                   map[0 -> RBYyk_].map[0 + RBYyk_ -> TFaRFNFOYxgT] *)

(* out SuDW_UD_S _ePHPJQp.map[0 -> TSbAMpTrE].filter[0 < TSbAMpTrE].map["" -> ZUfinT] *)
(* out SuDW_UD_S _ePHPJQp.map[0 -> TSbAMpTrE].filter[0 < TSbAMpTrE].map["" -> ZUfinT] *)
