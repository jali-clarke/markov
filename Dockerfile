FROM fpco/stack-build:lts-13.14 as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc

FROM scratch
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-13.14/8.6.4/bin/markov ./
CMD ["/markov", "5000"]