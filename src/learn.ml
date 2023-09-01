let () =
  let error fmt = Printf.ksprintf (fun s -> print_endline s; exit 1) fmt in
  let source = ref "" in
  let target = ref "" in
  let eta = ref 0.01 in
  let size = ref 10 in
  Arg.parse [
    "-s", Arg.Set_string source, "Source file.";
    "-t", Arg.Set_string target, "Target file.";
  ] (fun _ -> ()) "learn [options]";
  if !source = "" then error "Please specify an input file.";
  if !target = "" then error "Please specify a target file.";
  Random.self_init ();
  let source = WAV.openfile !source in
  let target = WAV.openfile !target in
  let net = Net.create (`WrightGRU !size) in
  for _ = 0 to 100000 do
    let x = WAV.sample_mean_float source in
    let yc = Net.process net x in
    let yt = WAV.sample_mean_float target in
    Printf.printf "S: %.02f\tT: %.02f\tC: %.02f\n%!" x yt yc;
    Net.descent net yt !eta
  done;
