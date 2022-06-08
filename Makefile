all: build

build:
	ghc --make Main.hs -o json-parser.exe -no-keep-hi-files -no-keep-o-files

clean:
	rm **/*.hi **/*.o ./json-parser.exe
