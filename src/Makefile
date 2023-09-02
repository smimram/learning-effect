all: test

test:
	./leffect -s source.wav -t target.wav -j test.json -r 0.0001

play:
	./leffect -s source.wav -j test.json -p -o /tmp/output.wav

muff:
	./leffect -s ../Automated-GuitarAmpModelling/Data/test/muff-input.wav -t ../Automated-GuitarAmpModelling/Data/test/muff-target.wav -r 0.0001 -j muff.json

muffplay:
	./leffect -i ../Automated-GuitarAmpModelling/Data/test/muff-input.wav -j muff.json -p -o ""
