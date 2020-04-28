open QCheck
open ExpressionGenerator

let nodes = function 
    | None -> -1
    | Some node -> match node with
        | Literal _ -> 0
        | Variable _ -> 1
        | OperatorApplication _ -> 2
        | ConditionalApplication _ -> 3

let rec size_literal_nodes = function 
    | None -> 0
    | Some node -> match node with 
      | Literal _ -> 1
      | Variable _ -> 0
      | OperatorApplication (n1, _, n2, _) -> size_literal_nodes (Some (n1)) + size_literal_nodes (Some(n2))
      | ConditionalApplication (n1, n2, _, n3, _) -> size_literal_nodes (Some(n1)) + size_literal_nodes (Some(n2)) + size_literal_nodes (Some(n3))

let rec size_variable_nodes = function 
      | None -> 0
      | Some node -> match node with 
        | Literal _ -> 0
        | Variable _ -> 1
        | OperatorApplication (n1, _, n2, _) -> size_variable_nodes (Some (n1)) + size_variable_nodes (Some(n2))
        | ConditionalApplication (n1, n2, _, n3, _) -> size_variable_nodes (Some(n1)) + size_variable_nodes (Some(n2)) + size_variable_nodes (Some(n3))
       
let fuel = 6     

let full_gen = make (expression_gen Integer global_scope fuel)

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
  Test.make ~count:10000 ~name:"integer root"
     integer_gen
     (fun _ -> true)

let exp_gen = expression_gen Integer global_scope fuel

let expression_gen_literal_statistics = 
   make ~stats:[("literal with fuel " ^ string_of_int fuel, size_literal_nodes)]
   (exp_gen)

let expression_gen_variable_statistics = 
   make ~stats:[("variable with fuel " ^ string_of_int fuel, size_variable_nodes)]
   (exp_gen)   

let literal_test = Test.make ~count:10000 ~name:"literal statistics three"
   expression_gen_literal_statistics (fun _ -> true)

let variable_test = Test.make ~count:10000 ~name:"variable statistics three"
   expression_gen_variable_statistics (fun _ -> true)

let _ = QCheck_runner.run_tests ~verbose:true
  [
    integer_test;
    literal_test;
    variable_test;
  ]
