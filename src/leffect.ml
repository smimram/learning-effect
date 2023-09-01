(** Pre-emphasis for error computation. *)
let preemph () =
  let x' = ref 0. in
  fun x ->
    let y = x -. 0.85 *. !x' in
    x' := y;
    y

let () =
  let error fmt = Printf.ksprintf (fun s -> print_endline s; exit 1) fmt in
  let source = ref "" in
  let target = ref "" in
  let output = ref "output.wav" in
  let json = ref "effect.json" in
  let rate = ref 0.005 in
  let size = ref 64 in
  Arg.parse [
    "-i", Arg.Set_string source, "Input file.";
    "-s", Arg.Set_string source, "Source file.";
    "-t", Arg.Set_string target, "Target file.";
    "-o", Arg.Set_string output, "Output file.";
    "--json", Arg.Set_string json, "JSON file.";
    "--rate", Arg.Set_float rate, "Learning rate.";
    "--size", Arg.Set_int size, "Size of the network."
  ] (fun _ -> ()) "learn [options]";
  if !source = "" then error "Please specify an input file.";
  if !target = "" then
    (
      let source = WAV.openfile !source in
      let channels = WAV.channels source in
      let samplerate = WAV.samplerate source in
      let samples = WAV.samples source in
      let output = WAV.Writer.openfile ~channels ~samplerate !output in
      let json = Yojson.Basic.from_file !json in
      let net = Array.init channels (fun _ -> Net.of_json json) in
      try
        let i = ref 0 in
        while true do
          Printf.printf "\rProcessing: %.00f%%%!" (100. *. float !i /. float samples);
          incr i;
          let x = WAV.sample_float source in
          let y = Array.init channels (fun c -> Net.process net.(c) x.(c)) in
          WAV.Writer.samples_float output y
        done
      with End_of_file -> Printf.printf "\rDone!\n%!"
    )
  else
    (
      Random.self_init ();
      let source = WAV.openfile !source in
      let target = WAV.openfile !target in
      let samples = WAV.samples source in
      let output = WAV.Writer.openfile ~channels:1 ~samplerate:(WAV.samplerate source) !output in
      let net = Net.create (`WrightGRU !size) in
      try
        let i = ref 0 in
        ignore (Sys.signal Sys.sigint (Signal_handle (fun _ -> raise End_of_file)));
        let pec = preemph () in
        let pet = preemph () in
        while true do
          Printf.printf "\rProcessing: %.00f%%%!" (100. *. float !i /. float samples);
          incr i;
          let x = WAV.sample_float source in
          let x = x.(0) in
          let yc = Net.process net x in
          let yt = WAV.sample_float target in
          let yt = yt.(0) in
          WAV.Writer.sample_float output yc;
          (* Printf.printf "S: %.02f\tT: %.02f\tC: %.02f\n" x yt yc; *)
          let yc = pec yc in
          let yt = pet yt in
          Net.descent net (!rate *. (yc -. yt))
        done;
      with
      | End_of_file ->
        Printf.printf "\rDone! Writing to %s.\n%!" !json;
        if !json <> "" then
          let oc = open_out !json in
          Net.to_json net |> Yojson.Basic.pretty_to_channel oc;
          close_out oc
    )
