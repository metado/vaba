FROM haskell:8.6.5

WORKDIR /opt/server

ADD . /opt/server

RUN cd /opt/server && cabal new-update && cabal new-build
