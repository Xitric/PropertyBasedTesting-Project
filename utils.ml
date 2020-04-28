(* Some useful serializers *)
let string_of_opt serializer = function
    | None -> "None"
    | Some var -> serializer var

let to_file serializer ast file_name =
    let out = open_out file_name in
    Printf.fprintf out "%s\n" (serializer ast);
    close_out out

let to_file_opt serializer ast file_name =
    let out = open_out file_name in
    Printf.fprintf out "%s\n" (string_of_opt serializer ast);
    close_out out

(* Some socket stuff *)
let read sock length =
    let buffer = Bytes.create length in
    match Unix.read sock buffer 0 length with
        | 0 -> raise Not_found
        | _ -> buffer

let read_all sock max =
    let buffer = Bytes.create max in
    let rec read_all_internal buffer ofs =
        let length = Unix.read sock buffer ofs (max - ofs) in
        let ofs = ofs + length in
        if Bytes.contains (Bytes.sub buffer 0 ofs) '\003' then
            Bytes.sub buffer 0 ofs
        else
            read_all_internal buffer ofs in
    read_all_internal buffer 0

let write_str sock payload =
    let byte_payload = Bytes.of_string payload in
    ignore (Unix.write sock byte_payload 0 (Bytes.length byte_payload))

let write_char sock payload =
    ignore (Unix.write sock (Bytes.make 1 payload) 0 1)
