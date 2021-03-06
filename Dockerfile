FROM fpco/stack-build:lts-13.14 as build
WORKDIR /opt/build
COPY stack.yaml .
RUN stack setup
COPY . .
RUN stack build

FROM ubuntu:16.04
RUN apt-get update \
    && apt-get install -y \
        libgmp3-dev \
        libssl1.0.0 \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /usr/local/bin
ARG TARGET
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-13.14/8.6.4/bin/${TARGET} ./
ENV EXEC_TARGET=${TARGET}
CMD ${EXEC_TARGET} 5000