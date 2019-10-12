#!/bin/bash

for target in markov-crud markov-sentence-generator
do
    docker build --build-arg TARGET=${target} -t docker.lan:5000/${target} .
    docker push docker.lan:5000/${target}
done