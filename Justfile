clean:
    cabal clean

build:
    cabal build

test:
    cabal test

run:
    cabal run flp-fun -- -p ./dummy-parser.py -t ./dummy-interpreter.py example_sol_tests | jq

all: clean build run