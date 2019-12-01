#!/bin/bash

docker build . -t docker.lan:5000/ditto \
    && docker push docker.lan:5000/ditto