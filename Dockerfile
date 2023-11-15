FROM haskell:latest

RUN stack setup --install-ghc

RUN echo "allow-newer: true" >> /root/.stack/config.yaml
RUN echo "allow-different-user: true" >> /root/.stack/config.yaml

COPY . /code

WORKDIR /code

RUN stack build --dependencies-only 

ENTRYPOINT "/bin/bash"
