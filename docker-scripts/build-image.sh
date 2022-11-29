#!/bin/bash

# run from repo root as docker-scripts/build-image.sh

mkdir -p build
stack install --local-bin-path ./build/

pushd data
./create-reference.sh
popd

docker build -t mfjval/pandoc-iso .
