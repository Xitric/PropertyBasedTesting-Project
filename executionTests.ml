open QCheck
open Utils
open PipelineGenerator
open PipelineModel
open Printf

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

let wrap_pipeline pipeline_ast =
    Printf.sprintf
    {|language python
channel endpoint
channel serial
board esp32 version wrover
    in serial
    sensor thermistor (12,13,14) as x(a,b,c)
        sample signal
        data result
            out endpoint x%s
|} (string_of_pipeline_node pipeline_ast)

let make_input_file a b c =
    let rec write_lines stream = function
        | (a::a_rest, b::b_rest, c::c_rest) ->
            fprintf stream "%d, %d, %d\n" a b c;
            write_lines stream (a_rest, b_rest, c_rest)
        | _ -> ()
    in
    let out_stream = open_out "test/src-gen/board/adc.csv" in
    try
        fprintf out_stream "a, b, c\n";
        write_lines out_stream (a, b, c);
        flush out_stream; true
    with e -> close_out_noerr out_stream; false

let model_agrees pipeline sock a b c =
    let expected_values = compute pipeline sock a b c in
    let in_stream = open_in "test/src-gen/board/endpoint.csv" in
    let rec check_recursive stream = function
        | expected::rest ->
            let actual = input_line stream in
            print_endline (actual ^ " = " ^ expected);
            actual = expected && check_recursive stream rest
        | _ -> true
    in check_recursive in_stream expected_values

(* Test that legal code generates the same output as a model *)
let pipeline_generator = make (pipeline_gen Integer [("a",Integer,21);("b",Integer,21);("c",Integer,21)] 6)
    ~print:string_of_pipeline_node
    ~shrink:(shrink_wrap (pipeline_shrinker [("a",Integer);("b",Integer);("c",Integer)]))

let _ = match Unix.fork () with
    | 0 ->
        (match Unix.fork () with
            | 0 ->
                Sys.command "cd test;java -jar iot-compiler.jar test.iot > out.txt"
            | pid ->
                ignore (Sys.command "rm groovy_server/execution_server/groovy-out.txt");
                ignore (Sys.command "cd groovy_server;ant compile");
                Sys.command "cd groovy_server/execution_server;groovy GroovyServer.groovy > groovy-out.txt")
    | pid ->
        let generator_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Unix.sleep 5;
        Unix.connect generator_socket (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 4000));

        let groovy_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Unix.sleep 5;
        Unix.connect groovy_socket (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 5000));
        
        ignore (Sys.command "mkdir -p test/src-gen/board");
        ignore (Sys.command "cp test_stubs/* test/src-gen/board");

        let compile_test = Test.make
            (* ~count:100 *)
            ~count:1
            ~name:"Generated Python software produces correct output"
            (pair
                (triple
                    (list_of_size (Gen.return 1000) small_signed_int)
                    (list_of_size (Gen.return 1000) small_signed_int)
                    (list_of_size (Gen.return 1000) small_signed_int)
                )
                pipeline_generator
            )
            (fun ((a, b, c), ast) ->
                (* Add when DSL actually works, because right now it is very broken *)
                let () = to_file wrap_pipeline ast "test/test.iot" in
                let generator_code = signal generator_socket in
                
                if generator_code = (Bytes.make 1 '\000') then (
                    ignore (Sys.command "cp test/src-gen/config.json test/src-gen/board/");
                    if make_input_file a b c then (
                        ignore (Sys.command "rm -f test/src-gen/board/endpoint.csv");
                        ignore (Sys.command "touch test/src-gen/board/endpoint.csv");
                        let python_code = Sys.command "cd test/src-gen/board;python3 main.py" in
                        if python_code = 0 then
                            match ast with
                                | Some pipeline ->
                                    model_agrees pipeline groovy_socket a b c
                                | None ->
                                    true
                        else false
                    ) else false
                ) else false) in
        
        let result = QCheck_runner.run_tests ~verbose:true [
            compile_test
        ] in
        stop generator_socket;
        write_str groovy_socket "kill";
        (* ignore (Sys.command "rm -r test/src-gen"); *)
        result
