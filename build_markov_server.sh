#!/bin/bash

for target in markov-crud markov-sentence-generator
do
    docker build --build-arg TARGET=${target} -t nexus.lan:5000/${target} .
    docker push nexus.lan:5000/${target}
done