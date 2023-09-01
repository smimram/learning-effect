# Machine learning audio effects

Trying to learn machine learning for audio effects. This is my own take at
coding the [guitar amplifier model based on
LSTMs](https://github.com/Alec-Wright/Automated-GuitarAmpModelling) (also
[described
here](https://towardsdatascience.com/neural-networks-for-real-time-audio-stateful-lstm-b534babeae5d)). In
OCaml.

## Usage

In order to learn an effect you need both an original sound file (say
`source.wav`) and a processed sound file (say `target.wav`). You can start
learning with

```shell
leffect -s source.wav -t target.wav
```

This will learn the effect. While doing so it will also produce an `output.wav`
file (name can be changed with `-o`) resulting of the processing of the input
file by the current network. Some other useful parameters are

- `--rate`: the learning rate
- `--size`: the size of the network
- `--play`: play the output in realtime

At the end, it produces an `effect.json` file (the name can be changed with the
`--json` flag).

In order to process a file with an already learned effect (whose parameters are
stored in `effect.json`), you can simply do

```shell
leffect -i file.wav -o output.wav
```
