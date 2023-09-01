type t =
  {
    ic : in_channel;
    channels : int;
    samplerate : int;
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
  let samplerate = input_int () in
  drop 4; (* audio data rate *)
  drop 2; (* block alignment *)
  let samplesize = input_short () in
  if input 4 <> "data" then raise Invalid;
  let length = input_int () in
  (* Printf.printf "%d / %d / %d / %d\n%!" channels samplereate samplesize length; *)
  if samplesize <> 16 then raise Unsupported;
  { ic; channels; samplerate; samplesize; length }

let channels wav = wav.channels

let samplerate wav = wav.samplerate

let samples wav = wav.length / (wav.channels * wav.samplesize / 8)

let sample wav =
  Array.init wav.channels (fun _ -> String.get_int16_le (really_input_string wav.ic 2) 0)

let sample_float wav =
  sample wav |> Array.map (fun x -> float x /. 32768.)

let sample_mean_float wav =
  sample_float wav |> Array.fold_left (+.) 0. |> fun x -> x /. float (channels wav)

module Writer = struct
  type t = out_channel

  let openfile ~channels ~samplerate fname : t =
    let oc = open_out fname in
    let w s = output_string oc s in
    let short n =
      let s = Bytes.create 2 in
      Bytes.set_uint16_le s 0 n;
      output_bytes oc s
    in
    let int n =
      let s = Bytes.create 4 in
      Bytes.set_int32_le s 0 (Int32.of_int n);
      output_bytes oc s
    in
    w "RIFF";
    int 0x7FFFFFFF; (* file size - 8 *)
    w "WAVE";
    w "fmt ";
    int 16;
    short 1;
    short channels;
    int samplerate;
    int (samplerate * channels * 2); (* bytes per second *)
    short 2; (* block alignment *)
    short 16; (* bits per sample *)
    w "data";
    int 0x7FFFFFFF; (* data length *)
    oc

  let sample wav x =
    let s = Bytes.create 2 in
    Bytes.set_int16_le s 0 x;
    output_bytes wav s

  let sample_float wav x =
    x *. 32768. |> int_of_float |> sample wav

  let samples_float wav x =
    Array.iter (sample_float wav) x
end
