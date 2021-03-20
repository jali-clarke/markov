#!/bin/bash

docker build . -t nexus.lan:5000/ditto:latest \
    && docker push nexus.lan:5000/ditto:latest