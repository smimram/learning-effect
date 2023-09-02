type t =
  {
    file : out_channel option;
    soundcard : Pulseaudio.Simple.t option;
  }

let create ~samplerate ~channels ?(filename="") ?(soundcard=false) () =
  let file = if filename = "" then None else Some (WAV.Writer.openfile ~channels ~samplerate filename) in
  let soundcard =
    if soundcard then
      let sample =
        { Pulseaudio.
          sample_format = Sample_format_float32le;
          sample_rate = samplerate;
          sample_chans = 1
        }
      in
      let pa = Pulseaudio.Simple.create ~client_name:"leffect" ~stream_name:"leffect" ~sample ~dir:Dir_playback () in
      Some pa
    else None
  in
  { file; soundcard }

let sample out x =
  Option.iter (fun wav -> WAV.Writer.samples_float wav x) out.file;
  Option.iter (fun pa -> Pulseaudio.Simple.write pa (Array.map (fun x -> [|x|]) x) 0 1) out.soundcard
