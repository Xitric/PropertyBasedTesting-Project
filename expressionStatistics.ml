open QCheck
open ExpressionGenerator

let nodes = function 
    | None -> -1
    | Some node -> match node with
        | Literal l -> 0
        | Variable (_, _) -> 1
        | OperatorApplication (_, _) -> 2
        | ConditionalApplication (_, _, _) -> 3

let full_gen = make (expression_gen Integer global_scope 6)

(* let expression_gen_statistics = make ~stats:[("expressions", nodes)] (expression_gen Integer global_scope 2) *)
let integer_test =     
  let node n = 
    if n = 0 then "literal"
    else if n = 1 then "variable" 
    else if n = 2 then "operator"
    else if n = 3 then "conditional"
    else "MISSING" in
  let integer_gen = set_collect
    (fun n -> node (nodes n))
    (full_gen) in
  Test.make ~count:10000 ~name:"integer tree_nodes"
     integer_gen
     (fun _ -> true)


let _ = QCheck_runner.run_tests ~verbose:true
  [
    integer_test;
  ]
