open QCheck
open Utils
open ScaffoldingGenerator

(* let get_signal sock =
    let buffer = Bytes.create 1 in
    match Unix.read sock buffer 0 1 with
        | 0 -> failwith "Received premature EOF"
        | _ -> buffer *)
(* let send sock payload =
    ignore (Unix.write sock (Bytes.make 1 payload) 0 1) *)
let signal sock =
    write_char sock '\000';
    read sock 1
let stop sock =
    write_char sock '\001';
    ignore (read sock 1)

(* Test that illegal code returns failure *)
(* let ast = generate1 (root_gen environment 6) in *)
let ast_generator = make (root_gen environment 6)

let _ = match Unix.fork () with
    | 0 -> Sys.command "java -jar iot-compiler.jar test/test.iot > test/out.txt"
    | pid ->
        let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Unix.sleep 5;
        Unix.connect socket (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 4000));
        
        let compile_test = Test.make
            ~count:100
            ~name:"IoT generator accepts legal code"
            ast_generator
            (fun ast ->
                let () = to_file string_of_dsl ast "test/test.iot" in
                let code = signal socket in
                (* let code = Sys.command "java -jar iot-compiler.jar test/test.iot" in *)
                (* code = 0 *)
                code = (Bytes.make 1 '\000')) in
        
        let result = QCheck_runner.run_tests ~verbose:true [
            compile_test
        ] in stop socket; result
