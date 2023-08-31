FROM ocaml/opam:alpine-3.16-ocaml-4.14 as build
USER 1000:1000
RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam
RUN cd ~/opam-repository && git pull origin -q master && git reset --hard e39502027c644408419cbba3f8bbbcfd71603476 && opam update
COPY --chown=opam tmf-pipeline.opam /src/
WORKDIR /src
RUN opam install -y --deps-only --with-test .
ADD --chown=opam . .
RUN opam exec -- dune build @runtest
RUN opam exec -- dune build --profile release ./_build/install/default/bin/retirement