(** Network models. *)

(** A network model. *)
type t =
  | WrightGRU of float array * GRU.t array * float ref (** GRU units followed by a fully connected network. *)

let create = function
  | `WrightGRU n ->
    let w = Array.init n (fun _ -> Random.float 2. -. 1.) in
    let g = Array.init n (fun _ -> GRU.create ()) in
    WrightGRU (w, g, ref 0.)

let process net x =
  match net with
  | WrightGRU (w, g, y) ->
    let out = ref 0. in
    let n = Array.length w in
    for i = 0 to n - 1 do
      out := !out +. w.(i) *. GRU.process g.(i) x
    done;
    y := !out;
    !out

let descent net y_expected eta =
  match net with
  | WrightGRU (w, g, y_computed) ->
    let n = Array.length w in
    let d = !y_computed -. y_expected in
    (* Printf.printf "d: %.02f\n%!" d; *)
    for i = 0 to n - 1 do
      w.(i) <- w.(i) -. eta *. d *. GRU.output g.(i);
      GRU.descent g.(i) (eta *. d *. w.(i))
    done
