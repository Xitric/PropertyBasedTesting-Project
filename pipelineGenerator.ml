open QCheck
open ExpressionGenerator

type primitive =
  | Integer
  | Float
  | Boolean
  | String

type value_type =
  | Primitive of primitive
  | Tuple of primitive list

type execute_pipeline =
  | Reduce
  | Mean
  | Median
  | Mode
  | Var
  | StDev
  | Min
  | Max
  | Count

type pipeline_node =
  | Filter of tree_node
  | Abs of tree_node
  | Map of tree_node * tree_node
  | Window of int * execute_pipeline

(* Scope for VarIDs *)
let variables = [
  (* E.g. *)
  (* ("a", Integer) *)
]

open Gen
(* Generator for variable identifiers *)
let identifier_gen =
  let identifier_char_gen = frequency [
    (8, char_range 'a' 'z');
    (8, char_range 'A' 'Z');
    (1, return '_');
  ] in
  let identifier_string_gen = string_size (int_bound 12) ~gen:(identifier_char_gen) in
  let rec identifier_gen_inner = function
    | "" | "_" -> identifier_string_gen >>= fun id -> identifier_gen_inner id
    | id -> return id in
  identifier_string_gen >>= fun id -> identifier_gen_inner id

(* Generator for types *)

(* Generator for execute pipelines *)

(* Generator for filter and abs *)  

(* Generator for map *)

(* Generator for window *)

(* Pipeline generator *)
