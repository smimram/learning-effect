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
  ignore (WAV.openfile "input.wav")
