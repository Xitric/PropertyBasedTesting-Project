open QCheck
open PipelineGenerator
open ExpressionGenerator

(* Types *)
type data_node = 
    | Out of string * string * pipeline_node

type sensor_node =
    | FrequencySample of int
    | SignalSample
    | Data of string * data_node list

type variables =
    | Variables of string * string list

type board_node =
    | In of string
    | ExtSensor of string * int list * variables * sensor_node list
    | OnbSensor of string * variables * sensor_node list

type root_node =
    | Language of string
    | Channel of string
    | Board of string * string * board_node list

type dsl =
    | Dsl of root_node list

let environment = ([], [], [])

(* Generators *)
open Gen
let rec channel_gen environment =
    list_size (int_range 1 6) identifier_gen >>= fun result ->
        let rec is_unique = function
            | [] -> true
            | elm::rest -> not (List.exists ((=) elm) rest) && is_unique rest
        in
        if is_unique result then
            return (List.map (fun str -> Channel str) result)
        else channel_gen environment

let rec out_gen environment =
    let single_out_gen (channel_env, source_name, variables) =
        oneofl channel_env >>= fun channel ->
            pipeline_gen variables 6 >>= function
                | None -> failwith "Error :'("
                | Some pipeline -> return (Out(channel, source_name, pipeline))
    in list_size (int_range 1 6) (single_out_gen environment)

let data_gen environment =
    identifier_gen >>= fun name ->
        out_gen environment >>= fun outs ->
            return (Data(name, outs))

let sample_gen =
    Gen.oneof [
        return SignalSample;
        small_nat >>= fun freq -> return (FrequencySample freq)
    ]

let variables_gen length =
    identifier_gen >>= fun name ->
        list_repeat length identifier_gen >>= fun vars ->
            return (Variables(name, vars))

let add_vars vars (channel_env, source_name, variables) = match vars with
    Variables(name, ids) ->
        List.fold_left
            (fun (channel_env, source_name, variables) id ->
                (channel_env, source_name, (id, Integer, 21)::variables))
            (channel_env, name, variables)
            ids

let sensor_gen environment =
    let ext_sensor_gen environment sampler =
        identifier_gen >>= fun name ->
            int_range 1 4 >>= fun length ->
                list_repeat length small_nat >>= fun pins ->
                    variables_gen length >>= fun vars ->
                        let new_environment = add_vars vars environment in
                        data_gen new_environment >>= fun datas ->
                            return (ExtSensor(name, pins, vars, [sampler;datas])) in
    (* let onb_sensor_gen environment = ? in *)
    
    sample_gen >>= fun sampler ->
        oneof [
            ext_sensor_gen environment sampler;
            (* onb_sensor_gen environment *)
        ]

let in_gen (channel_env, _, _) =
    oneofl channel_env >>= fun channel -> return (In channel)

let board_gen environment fuel =
    oneofl["esp32"; "ESP32"] >>= fun name ->
    oneofl["wrover"] >>= fun version ->
    
    let board_nodes_gen nodes fuel =
        (* if fuel = 0 then *)
            sensor_gen environment >>= fun sensor ->
            in_gen environment >>= fun in_ -> 
            return (Board (name, version, in_::sensor::nodes)) in
    board_nodes_gen [] 23
        (* else 
            
                return board_nodes_gen nodes::sensor fuel / 2) *)

let add_channels channels (channel_env, source_name, variables) =
    List.fold_left
        (fun (channel_env, source_name, variables) -> function
            | Channel name -> (name::channel_env, source_name, variables)
            | _ -> (channel_env, source_name, variables))
        (channel_env, source_name, variables)
        channels

let root_gen environment fuel =  
    channel_gen environment >>= fun channels ->
        let new_environment = add_channels channels environment in
        board_gen new_environment fuel >>= fun board ->
            return (Dsl([Language "python"] @ channels @ [board]))

(* Serializers *)
let string_of_data_node = function
    | Out(channel, source, pipeline) -> "\t\t\t\tout " ^ channel ^ " " ^ source ^ (string_of_pipeline_node (Some pipeline) ^ "\n")

let string_of_sensor_node = function
    | FrequencySample freq -> "\t\t\tsample frequency " ^ (string_of_int freq) ^ "\n"
    | SignalSample -> "\t\t\tsample signal\n"
    | Data(name, outs) -> "\t\t\tdata " ^ name ^ "\n" ^
        String.concat "" (List.map string_of_data_node outs)

let string_of_variables = function
    | Variables(name, ids) -> " as " ^ name ^ "(" ^
        String.concat ", " ids
    ^ ")"

let string_of_board_node = function
    | In channel -> "\tin " ^ channel ^ "\n"
    | ExtSensor(name, pins, vars, body) -> "\tsensor " ^ name ^ "(" ^
        String.concat ", " (List.map string_of_int pins)
    ^ ")" ^ (string_of_variables vars) ^ "\n" ^
        String.concat "" (List.map string_of_sensor_node body)
    | OnbSensor(type_name, vars, body) -> failwith "Unsupported"

let string_of_root_node = function
    | Language lang -> "language " ^ lang ^ "\n"
    | Channel name -> "channel " ^ name ^ "\n"
    | Board(name, version, body) -> "board " ^ name ^ " version " ^ version ^ "\n" ^
        String.concat "" (List.map string_of_board_node body)

let string_of_dsl = function
    | Dsl content -> String.concat "" (List.map string_of_root_node content)

let generate =
    let ast = generate1 (root_gen environment 6) in
    let out = open_out "generated.iot" in
    Printf.fprintf out "%s\n" (string_of_dsl ast);
    close_out out
