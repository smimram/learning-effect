type t =
  {
    ic : in_channel;
    channels : int;
    samplereate : int;
    samplesize : int;
    length : int
  }

exception Invalid
  exception Unsupported

let openfile fname =
  let ic = open_in fname in
  let input n = really_input_string ic n in
  let drop n = ignore (input n) in
  let input_short () = String.get_uint16_le (input 2) 0 in
  let input_int () = String.get_int32_le (input 4) 0 |> Int32.to_int in
  if input 4 <> "RIFF" then raise Invalid;
  drop 4; (* file size *)
  if input 4 <> "WAVE" then raise Invalid;
  if input 4 <> "fmt " then raise Invalid;
  drop 4; (* size of the chunk *)
  if input_short () <> 1 then raise Unsupported;
  let channels = input_short () in
  let samplereate = input_int () in
  drop 4; (* audio data rate *)
  drop 2; (* block alignment *)
  let samplesize = input_short () in
  if input 4 <> "data" then raise Invalid;
  let length = input_int () in
  (* Printf.printf "%d / %d / %d / %d\n%!" channels samplereate samplesize length; *)
  if samplesize <> 16 then raise Unsupported;
  { ic; channels; samplereate; samplesize; length }

let channels wav =
  wav.channels

let sample wav =
  Array.init wav.channels (fun _ -> String.get_int16_le (really_input_string wav.ic 2) 0)

let sample_float wav =
  sample wav |> Array.map (fun x -> float x /. 32768.)

let sample_mean_float wav =
  sample_float wav |> Array.fold_left (+.) 0. |> fun x -> x /. float (channels wav)
