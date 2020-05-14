open QCheck
open Utils
open PipelineGenerator
open TraverseSyntaxTree

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

let pipeline_generator = make (pipeline_gen Integer [("a",Integer,21);("b",Integer,21);("c",Integer,21)] 6) ~print:string_of_pipeline_node 

let _ = match Unix.fork () with
    | 0 -> Sys.command "java -jar iot-compiler.jar test/exceptionTest.iot > test/out.txt"
    | pid ->
        let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Unix.sleep 5;
        Unix.connect socket (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 4000));

        let exception_test = Test.make
            ~count:100
            ~name:"Exception tests"
            pipeline_generator
            (fun pipe -> 
                let () = to_file wrap_pipeline pipe "test/exceptionTest.iot" in
                let code = signal socket in
                code = (Bytes.make 1 '\000')) in
            
                let result = QCheck_runner.run_tests ~verbose:true [
                    exception_test
                ] in
                stop socket;
                ignore (Sys.command "rm -r src-gen");
                result
                