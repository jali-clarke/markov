FROM fpco/stack-build:lts-14.8 as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc

FROM scratch
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-14.8/8.6.5/bin/markov-server ./
CMD ["/markov-server", "5000"]