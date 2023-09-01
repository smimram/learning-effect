let () =
  let error fmt = Printf.ksprintf (fun s -> print_endline s; exit 1) fmt in
  let source = ref "" in
  let target = ref "" in
  let output = ref "output.wav" in
  let json = ref "effect.json" in
  let rate = ref 0.1 in
  let size = ref 20 in
  Arg.parse [
    "-s", Arg.Set_string source, "Source file.";
    "-t", Arg.Set_string target, "Target file.";
    "-o", Arg.Set_string output, "Output file.";
    "--json", Arg.Set_string json, "JSON file.";
    "--rate", Arg.Set_float rate, "Learning rate.";
    "--size", Arg.Set_int size, "Size of the network."
  ] (fun _ -> ()) "learn [options]";
  if !source = "" then error "Please specify an input file.";
  if !target = "" then error "Please specify a target file.";
  Random.self_init ();
  let source = WAV.openfile !source in
  let target = WAV.openfile !target in
  let samples = WAV.samples source in
  let output = WAV.Writer.openfile ~channels:1 ~samplerate:(WAV.samplerate source) !output in
  let net = Net.create (`WrightGRU !size) in
  try
    let i = ref 0 in
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
      Net.descent net yt !rate
    done;
  with
  | End_of_file ->
    Printf.printf "\rDone!\n%!";
    if !json <> "" then
      let oc = open_out !json in
      Net.to_json net |> Yojson.Basic.pretty_to_channel oc;
      close_out oc
