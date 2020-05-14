open QCheck
open Utils
open PipelineGenerator
open ExpressionGenerator

let wrap_pipeline pipeline_ast =
    Printf.sprintf
    {|language python
channel endpoint
board esp32 version wrover
    sensor thermistor (12,13,14) as x(a,b,c)
        data result
            out endpoint x%s
|} (string_of_pipeline_node pipeline_ast)


(* let pipepline_generator = make (pipeline_gen Integer [("a",Integer,21);("b",Integer,21);("c",Integer,21)] 6) ~print:string_of_pipeline_node  *)
let rec getExpressions pipeline expressions = match pipeline with
| None -> expressions
| Some pipeline -> match pipeline with
  | Filter (tree_node,pipeline_node) -> getExpressions pipeline_node (expressions@[string_of_tree_node tree_node])
  | Map (tree_node, _, pipeline_node) -> getExpressions pipeline_node (expressions@[string_of_tree_node tree_node])
  | Window (_, execute_pipeline, pipeline_node) -> getExpressions pipeline_node expressions

let rec printExpressions = function
  | [] -> []
  | expression::rest -> print_endline expression; printExpressions rest

let pipe = (Gen.generate1 (pipeline_gen Integer [("a",Integer,21);("b",Integer,21);("c",Integer,21)] 6))

(* printExpressions (getExpressions pipe [] ) *)



