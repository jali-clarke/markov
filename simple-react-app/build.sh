#!/bin/bash

stack build markov-api-generator:exe:api-generator \
    && stack exec api-generator src/api \
&& docker build . -t nexus.lan:5000/simple-react-app \
    && docker push nexus.lan:5000/simple-react-app