FROM ocaml/opam:alpine

USER root

ENV OPAMYES=1

RUN apk add --no-cache git gcc m4 && \
    opam init && \
    opam install ocamlfind menhir && \
    eval `opam config env` && \
    git clone https://github.com/ChrisLane/Compiler-Construction /compiler && \
    cd /compiler && \
    make && \
    apk del --no-cache git

WORKDIR /compiler
