(** Network models. *)

(** A network model. *)
type t =
  | WrightGRU of float array * GRU.t array (** GRU units followed by a fully connected network. *)

(** Create a network. *)
let create = function
  | `WrightGRU n ->
    let w = Array.init n (fun _ -> Random.float 2. -. 1.) in
    let g = Array.init n (fun _ -> GRU.create ()) in
    WrightGRU (w, g)

(** Process a sample. *)
let process net x =
  match net with
  | WrightGRU (w, g) ->
    let out = ref 0. in
    let n = Array.length w in
    for i = 0 to n - 1 do
      out := !out +. w.(i) *. GRU.process g.(i) x
    done;
    !out

(** Apply gradient descent. *)
let descent net eta =
  match net with
  | WrightGRU (w, g) ->
    let n = Array.length w in
    (* Printf.printf "d: %.02f\n%!" d; *)
    for i = 0 to n - 1 do
      w.(i) <- w.(i) -. eta *. GRU.output g.(i);
      GRU.descent g.(i) (eta *. w.(i))
    done

(** Export to JSON. *)
let to_json = function
  | WrightGRU (w, g) ->
    `Assoc [
      "model", `String "wright-gru";
      "w", `List (Array.to_list w |> List.map (fun w -> `Float w));
      "gru", `List (Array.to_list g |> List.map GRU.to_json)
    ]

(** Import from JSON. *)
let of_json json =
  let open Yojson.Basic.Util in
  let json = to_assoc json in
  let kind = List.assoc "model" json |> to_string in
  match kind with
  | "wright-gru" ->
    let w = List.assoc "w" json |> to_list |> List.map to_float |> Array.of_list in
    let g = List.assoc "gru" json |> to_list |> List.map GRU.of_json |> Array.of_list in
    assert (Array.length w = Array.length g);
    WrightGRU (w, g)
  | _ -> failwith ("Unexpected kind " ^ kind)
