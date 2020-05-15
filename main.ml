open QCheck
open Utils
open ScaffoldingGenerator

let signal sock =
    write_char sock '\000';
    try
        read sock 1
    with Not_found -> (Bytes.make 1 '\001')
let stop sock =
    write_char sock '\001';
    try
        ignore (read sock 1)
    with Not_found -> ()

(* Test that legal code is parsed without errors *)
let ast_generator = make (root_gen environment 6) ~print:string_of_dsl ~shrink:dsl_shrinker

let _ = match Unix.fork () with
    | 0 -> Sys.command "java -jar iot-compiler.jar test/test.iot > test/out.txt"
    | pid ->
        let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Unix.sleep 5;
        Unix.connect socket (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 4000));
        
        let compile_test = Test.make
            (* ~count:100 *)
            ~count:1
            ~name:"IoT generator accepts legal code"
            ast_generator
            (fun ast ->
                (* let () = to_file string_of_dsl ast "test/test.iot" in *)
                let code = signal socket in
                code = (Bytes.make 1 '\000')) in
        
        (* QCheck_runner.set_seed 528671099; *)
     
        (* QCheck_runner.set_seed 364960045; *)
        (* 390736775 *)
        (* 187700661 *)
        
        let result = QCheck_runner.run_tests ~verbose:true [
            compile_test
        ] in
        stop socket;
        (* ignore (Sys.command "rm -r src-gen"); *)
        result
