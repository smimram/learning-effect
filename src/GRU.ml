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
