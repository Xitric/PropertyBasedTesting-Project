open QCheck
open ExpressionGenerator

let rec size_literal_nodes = function 
    | None -> 0
    | Some node -> match node with 
      | Literal _ -> 1
      | Variable _ -> 0
      | OperatorApplication (n1, _, n2, _) -> size_literal_nodes (Some (n1)) + size_literal_nodes (Some(n2))
      | ConditionalApplication (n1, n2, n3, _) -> size_literal_nodes (Some(n1)) + size_literal_nodes (Some(n2)) + size_literal_nodes (Some(n3))

let rec size_variable_nodes = function 
      | None -> 0
      | Some node -> match node with 
        | Literal _ -> 0
        | Variable _ -> 1
        | OperatorApplication (n1, _, n2, _) -> size_variable_nodes (Some (n1)) + size_variable_nodes (Some(n2))
        | ConditionalApplication (n1, n2, n3, _) -> size_variable_nodes (Some(n1)) + size_variable_nodes (Some(n2)) + size_variable_nodes (Some(n3))

let rec size_operator_application_nodes = function 
      | None -> 0
      | Some node -> match node with 
        | Literal _ -> 0
        | Variable _ -> 0
        | OperatorApplication (n1, _, n2, _) -> size_variable_nodes (Some (n1)) + size_variable_nodes (Some(n2)) + 1
        | ConditionalApplication (n1, n2, n3, _) -> size_variable_nodes (Some(n1)) + size_variable_nodes (Some(n2)) + size_variable_nodes (Some(n3))

let rec size_conditional_application_nodes = function 
        | None -> 0
        | Some node -> match node with 
          | Literal _ -> 0
          | Variable _ -> 0
          | OperatorApplication (n1, _, n2, _) -> size_variable_nodes (Some (n1)) + size_variable_nodes (Some(n2))
          | ConditionalApplication (n1, n2, n3, _) -> size_variable_nodes (Some(n1)) + size_variable_nodes (Some(n2)) + size_variable_nodes (Some(n3)) + 1
       
let fuel = 6     

let exp_gen = expression_gen Integer global_scope fuel

let expression_gen_literal_statistics = 
   make ~stats:[("literal with fuel " ^ string_of_int fuel, size_literal_nodes)] (exp_gen)

let expression_gen_variable_statistics = 
   make ~stats:[("variable with fuel " ^ string_of_int fuel, size_variable_nodes)] (exp_gen)  

let expression_gen_literal_statistics = 
   make ~stats:[("operator application with fuel " ^ string_of_int fuel, size_operator_application_nodes)] (exp_gen)
 
let expression_gen_variable_statistics = 
   make ~stats:[("conditional application with fuel " ^ string_of_int fuel, size_conditional_application_nodes)] (exp_gen)   

let literal_test = Test.make ~count:10000 ~name:"literal statistics three"
   expression_gen_literal_statistics (fun _ -> true)

let variable_test = Test.make ~count:10000 ~name:"variable statistics three"
   expression_gen_variable_statistics (fun _ -> true)

let operator_application_test = Test.make ~count:10000 ~name:"operator application statistics three"
   expression_gen_literal_statistics (fun _ -> true)

let conditional_application_test = Test.make ~count:10000 ~name:"conditional application statistics three"
   expression_gen_variable_statistics (fun _ -> true)


let _ = QCheck_runner.run_tests ~verbose:true
  [
    (* literal_test; *)
    variable_test;
    (* operator_application_test;
    conditional_application_test; *)
  ]
