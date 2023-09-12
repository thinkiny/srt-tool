all: srt-tools

srt-tools: SrtTools.scala
	scala-cli package $? . -f

clean:
	rm srt-tools

.PHONY: clean
