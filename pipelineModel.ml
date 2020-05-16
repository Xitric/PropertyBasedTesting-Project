open ExpressionGenerator
open PipelineGenerator
open Utils

let success_code = '\048'
let arithmetic_code = '\049'
let failure_code = '\050'

let execute_expression sock expression =
    write_str sock (expression ^ "\n");
    let response = read sock 1 in
    match Bytes.get response 0 with
        | n when n = success_code ->
            let result = read_all sock 512 in
            Some(Bytes.to_string result)
        | n when n = arithmetic_code ->
            None
        | n when n = failure_code ->
            failwith "Computation failed"
        | n -> failwith ("Unexpected response " ^ (Bytes.to_string response))

let compute_expression expression scope sock =
    let rec serialize_parameters = function
        | (id, typ, value)::rest ->
            "def " ^ id ^ " = " ^
            (if typ = String then "\"" ^ value ^ "\"" else value)
            ^ ";" ^
            serialize_parameters rest
        | _ -> ""
    in
    let program = serialize_parameters scope ^ string_of_tree_node expression in
    (* TODO: Run with Groovy *)
    match execute_expression sock program with
        | Some value -> value
        | None -> "Arithmetic error"

let compute_boolean_expression expression scope sock =
    bool_of_string (compute_expression expression scope sock)

let first_value = function
    | (_, _, v)::_ -> v
    | _ -> raise Not_found

let compute_single pipeline sock a b c =
    let rec handle pipeline scope = match pipeline with
        | Filter(expression, next) ->
            if compute_boolean_expression expression scope sock then (
                match next with
                    | Some n -> handle n scope
                    | None -> Some(first_value scope)
            ) else None
        | Map(expression, output, next) ->
            let new_value = compute_expression expression scope sock in
            let new_scope = [output, resolves_to expression, new_value] in
            (match next with
                | Some n -> handle n new_scope
                | None -> Some(first_value new_scope))
        | Window(width, execute, next) -> None
    in handle pipeline [("a", Integer, a);("b", Integer, b);("c", Integer, c)]

let compute pipeline sock a b c =
    let rec compute_loop pipeline acc = function
        | (a::a_rest, b::b_rest, c::c_rest) -> (
            match compute_single pipeline sock a b c with
                | Some value ->
                    compute_loop pipeline (acc@[value]) (a_rest, b_rest, c_rest)
                | None ->
                    compute_loop pipeline acc (a_rest, b_rest, c_rest))
        | _ -> acc
    in compute_loop pipeline [] (
        List.map (fun v -> string_of_int v) a,
        List.map (fun v -> string_of_int v) b,
        List.map (fun v -> string_of_int v) c)
