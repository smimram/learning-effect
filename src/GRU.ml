(** Gated recurrent units. *)

type t =
  {
    mutable vr : float;
    mutable wr : float;
    mutable br : float;
    mutable vu : float;
    mutable wu : float;
    mutable bu : float;
    mutable vc : float;
    mutable wc : float;
    mutable bc : float;
    mutable x  : float;
    mutable r  : float;
    mutable u  : float;
    mutable c  : float;
    mutable h  : float;
  }

let create () =
  let init () = Random.float 2. -. 1. in
  {
    vr = init ();
    wr = init ();
    br = 0.;
    vu = init ();
    wu = init ();
    bu = 0.;
    vc = init ();
    wc = init ();
    bc = 0.;
    x  = 0.;
    r  = 0.;
    u  = 0.;
    c  = 0.;
    h  = 0.
  }

(** Export to JSON. *)
let to_json net = `Assoc [
    "vr", `Float net.vr;
    "wr", `Float net.wr;
    "br", `Float net.br;
    "vu", `Float net.vu;
    "wu", `Float net.wu;
    "bu", `Float net.bu;
    "vc", `Float net.vc;
    "wc", `Float net.wc;
    "bc", `Float net.bc;
  ]

(** Create from JSON. *)
let of_json json =
  let open Yojson.Basic.Util in
  let json = to_assoc json in
  {
    vr = List.assoc "vr" json |> to_float;
    wr = List.assoc "wr" json |> to_float;
    br = List.assoc "br" json |> to_float;
    vu = List.assoc "vu" json |> to_float;
    wu = List.assoc "wu" json |> to_float;
    bu = List.assoc "bu" json |> to_float;
    vc = List.assoc "vc" json |> to_float;
    wc = List.assoc "wc" json |> to_float;
    bc = List.assoc "bc" json |> to_float;
    x = 0.;
    r = 0.;
    u = 0.;
    c = 0.;
    h = 0.
  }

(** Process one input. *)
let process net x =
  let r = ML.sigmoid (net.vr *. x +. net.wr *. net.h +. net.br) in
  let u = ML.sigmoid (net.vu *. x +. net.wu *. net.h +. net.bu) in
  let c = tanh (net.vc *. x +. net.wc *.r *. net.h +. net.bc) in
  let y = (1. -. u) *. net.h +. u *. c in
  net.x <- x;
  net.r <- r;
  net.u <- u;
  net.c <- c;
  net.h <- y;
  y

(** Last computed output. *)
let output net = net.h

(** Gradient descent. *)
let descent net eta =
  (* Printf.printf "eta: %.02f\n%!" eta; *)
  let c' = net.u *. (1. -. net.c *. net.c) in
  let r' = c' *. net.wc *. net.h *. net.r *. (1. -. net.r) in
  let u' = (net.c -. net.h) *. net.u *. (1. -. net.u) in
  net.vr <- net.vr -. eta *. r' *. net.x;
  net.wr <- net.wr -. eta *. r' *. net.h;
  net.br <- net.br -. eta *. r';
  net.vu <- net.vu -. eta *. u' *. net.x;
  net.wu <- net.wu -. eta *. u' *. net.h;
  net.bu <- net.bu -. eta *. u';
  net.vc <- net.vc -. eta *. c' *. net.x;
  net.wc <- net.wc -. eta *. c' *. net.r *. net.h;
  net.bc <- net.bc -. eta *. c'
