open QCheck
open PipelineGenerator
open ExpressionGenerator

(* Types *)
type data_node = 
    | Out of string * string * pipeline_node

type sensor_node =
    | Data of string * data_node list

type sampler_node =
    | FrequencySample of int
    | SignalSample

type variables =
    | Variables of string * string list

type board_node =
    | In of string
    | ExtSensor of string * int list * variables * sampler_node * sensor_node list
    | OnbSensor of string * variables * sampler_node * sensor_node list

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
    let single_out_gen out_type (channel_env, source_name, variables) =
        oneofl channel_env >>= fun channel ->
            pipeline_gen out_type variables 25 >>= function
                | None -> failwith "Error :'("
                | Some pipeline -> return (Out(channel, source_name, pipeline))
    in type_gen >>= fun out_type ->
        list_size (int_range 1 12) (single_out_gen out_type environment)

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
                            return (ExtSensor(name, pins, vars, sampler, [datas])) in
    
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
    | Out(channel, source, pipeline) -> "\t\t\tout " ^ channel ^ " " ^ source ^ (string_of_pipeline_node (Some pipeline) ^ "\n")

let string_of_sensor_node = function
    | Data(name, outs) -> "\t\tdata " ^ name ^ "\n" ^
        String.concat "" (List.map string_of_data_node outs)

let string_of_sampler_node = function
    | FrequencySample freq -> "\t\tsample frequency " ^ (string_of_int freq) ^ "\n"
    | SignalSample -> "\t\tsample signal\n"

let string_of_variables = function
    | Variables(name, ids) -> " as " ^ name ^ "(" ^
        String.concat ", " ids
    ^ ")"

let string_of_board_node = function
    | In channel -> "\tin " ^ channel ^ "\n"
    | ExtSensor(name, pins, vars, sampler, body) -> "\tsensor " ^ name ^ "(" ^
        String.concat ", " (List.map string_of_int pins)
    ^ ")" ^ (string_of_variables vars) ^ "\n" ^
        string_of_sampler_node sampler ^
        String.concat "" (List.map string_of_sensor_node body)
    | OnbSensor(type_name, vars, sampler, body) -> failwith "Unsupported"

let string_of_root_node = function
    | Language lang -> "language " ^ lang ^ "\n"
    | Channel name -> "channel " ^ name ^ "\n"
    | Board(name, version, body) -> "board " ^ name ^ " version " ^ version ^ "\n" ^
        String.concat "" (List.map string_of_board_node body)

let string_of_dsl = function
    | Dsl content -> String.concat "" (List.map string_of_root_node content)

(* Shrinker *)
let make_scope = function
    Variables(_, vars) -> List.map (fun v -> (v, Integer)) vars

let (>>=) = Iter.(>>=)
let non_empty_list_shrinker elem_shrinker = function
    | [] -> Iter.empty
    | a::[] as list -> Shrink.list_elems elem_shrinker list
    | list -> Shrink.list ~shrink:elem_shrinker list >>= function
        | [] -> Iter.return [List.hd list]
        | list' -> Iter.return list'

let data_node_shrinker scope = function
    | Out(channel, id, pipeline) ->
        Iter.map (fun pipeline' -> Out(channel, id, pipeline')) (pipeline_shrinker scope pipeline)

let sensor_node_shrinker scope = function
    | Data(id, outs) ->
        Iter.map (fun outs' -> Data(id, outs')) (non_empty_list_shrinker (data_node_shrinker scope) outs)

let board_node_shrinker = function
    | In _ -> Iter.empty
    | ExtSensor(id, pins, vars, sampler, datas) ->
        Iter.map (fun datas' -> ExtSensor(id, pins, vars, sampler, datas')) (non_empty_list_shrinker (sensor_node_shrinker (make_scope vars)) datas)
        (* TODO: Shrink pins and vars, if they are not used in the pipeline *)
        (* Thus, we should skrink the pipelines first, as we do here *)
    | OnbSensor(id, vars, sampler, datas) ->
        Iter.map (fun datas' -> OnbSensor(id, vars, sampler, datas')) (non_empty_list_shrinker (sensor_node_shrinker (make_scope vars)) datas)
        (* TODO: same as above *)

let root_node_shrinker = function
    | Language l -> Iter.empty
    (* TODO: If we shrink channel names, we also have to replace the names throughout the entire AST *)
    | Channel ch -> Iter.empty
    | Board(name, version, sensors) ->
        Iter.map (fun sensors' -> Board(name, version, sensors')) (non_empty_list_shrinker board_node_shrinker sensors)

let dsl_shrinker = function
    | Dsl(content) ->
        Iter.map (fun content' -> Dsl(content')) (Shrink.list_elems root_node_shrinker content)

(* TODO: *)
(*
type variables =
    | Variables of string * string list
*)
