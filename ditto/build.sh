#!/bin/bash

docker build . -t docker.lan:5000/ditto:latest \
    && docker push docker.lan:5000/ditto:latest