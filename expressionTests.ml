open QCheck
open Utils
open ExpressionGenerator

let int_exp_gen = make (expression_gen Integer global_scope 20)
let float_exp_gen = make (expression_gen Float global_scope 20)
let bool_exp_gen = make (expression_gen Boolean global_scope 20)
let str_exp_gen = make (expression_gen String global_scope 20)

let success_code = '\048'
let arithmetic_code = '\049'
let failure_code = '\050'

(* We ignore errors like these, because they are expected with our generator *)
(*      Division by zero *)
(*      Division undefined 0/0 *)
(*      Timeouts due to extreme exponentiation *)
(* We just want to know if the expressions compile *)
let execute_expression sock expression =
    write_str sock (expression ^ "\n");
    let response = read sock 1 in
    match Bytes.get response 0 with
        | n when n = success_code ->
            (* let _ = read_all sock 512 in *)
            true
        | n when n = arithmetic_code ->
            true
        | n when n = failure_code ->
            false
        | n -> failwith ("Unexpected response " ^ (Bytes.to_string response))

let _ = match Unix.fork () with
    | 0 ->
        ignore (Sys.command "rm test/groovy-out.txt");
        Sys.command "groovy groovy-server.groovy > test/groovy-out.txt"
    | pid ->
        let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Unix.sleep 5;
        Unix.connect socket (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 5000));

        let int_exp_test = Test.make
            ~count:10000
            ~name:"Integer expressions are valid in Groovy"
            int_exp_gen
            (fun ast ->
                let expression = string_of_opt string_of_tree_node ast in
                execute_expression socket expression) in
        
        let float_exp_test = Test.make
            ~count:10000
            ~name:"Float expressions are valid in Groovy"
            float_exp_gen
            (fun ast ->
                let expression = string_of_opt string_of_tree_node ast in
                execute_expression socket expression) in
        
        let bool_exp_test = Test.make
            ~count:10000
            ~name:"Boolean expressions are valid in Groovy"
            bool_exp_gen
            (fun ast ->
                let expression = string_of_opt string_of_tree_node ast in
                execute_expression socket expression) in
        
        let str_exp_test = Test.make
            ~count:10000
            ~name:"String expressions are valid in Groovy"
            str_exp_gen
            (fun ast ->
                let expression = string_of_opt string_of_tree_node ast in
                execute_expression socket expression) in

        let result = QCheck_runner.run_tests ~verbose:true [
            int_exp_test;
            float_exp_test;
            bool_exp_test;
            str_exp_test
        ] in write_str socket "kill"; result
