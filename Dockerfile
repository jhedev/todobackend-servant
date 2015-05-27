FROM heroku/cedar:14

ENV GHCVER 7.8.4
ENV CABALVER 1.18

RUN apt-get update && apt-get install -y --no-install-recommends software-properties-common \
  && add-apt-repository -y ppa:hvr/ghc \
  && apt-get update \
  && apt-get install -y --no-install-recommends \
      cabal-install-$CABALVER \
      ghc-$GHCVER \
  && rm -rf /var/lib/apt/lists/*

ENV PATH /opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH

RUN useradd -d /app -m app
USER app
WORKDIR /app

ENV HOME /app
ENV PORT 3000

RUN mkdir -p /app/heroku
RUN mkdir -p /app/src
RUN mkdir -p /app/.profile.d

ONBUILD COPY . /app/src

ONBUILD USER root
ONBUILD RUN chown -R app /app/src
ONBUILD USER app

ONBUILD WORKDIR /app/src
ONBUILD RUN cabal update
ONBUILD RUN cabal install

ONBUILD RUN mkdir -p /app/target && cp $HOME/.cabal/bin/todobackend-servant /app/target/todobackend-servant

# Cleanup to make slug smaller
ONBUILD RUN rm -rf /app/src /app/.cabal /app/.ghc

ONBUILD EXPOSE 3000
