#!/bin/bash

stack build markov-api-generator:exe:api-generator \
    && stack exec api-generator src/api \
&& docker build . -t docker.lan:5000/simple-react-app \
    && docker push docker.lan:5000/simple-react-app