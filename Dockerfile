FROM ocaml/opam:alpine-3.16-ocaml-4.14 as build
USER 1000:1000
RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam
RUN cd ~/opam-repository && git pull origin -q master && git reset --hard 7dbbdf38edcb4e6e73461f0e7bb2ada6c9314c2f && opam update
COPY --chown=opam tmf-pipeline.opam /src/
WORKDIR /src
RUN opam install -y --deps-only --with-test .
ADD --chown=opam . .
RUN opam exec -- dune build @runtest @install @check