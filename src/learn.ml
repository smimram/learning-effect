let () =
  let error fmt = Printf.ksprintf (fun s -> print_endline s; exit 1) fmt in
  let source = ref "" in
  let target = ref "" in
  Arg.parse [
    "-s", Arg.Set_string source, "Source file.";
    "-t", Arg.Set_string target, "Target file.";
  ] (fun _ -> ()) "learn [options]";
  if !source = "" then error "Please specify an input file.";
  if !target = "" then error "Please specify a target file.";
  Random.self_init ();
  let source = WAV.openfile !source in
  let target = WAV.openfile !target in
  for _ = 0 to 100000 do
    let x = WAV.sample_mean_float source in
    let yt = WAV.sample_mean_float target in
    Printf.printf "%.02f -> %.02f\n%!" x yt
  done;
  ignore (GRU.create ())
