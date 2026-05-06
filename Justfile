clean:
    cabal clean

build:
    cabal build

test:
    cabal test

run: build
    cabal run flp-fun -- -o report.json -p ./dummy-parser.py -t ./dummy-interpreter.py example_sol_tests | jq

all: clean build run