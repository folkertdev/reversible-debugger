#
# Buils stage
#
FROM fpco/stack-build:lts-9.9 
RUN apt-get update -y && apt-get install -y nodejs
RUN stack upgrade
RUN stack setup 

# common haskell packages
RUN stack install --resolver lts-9.9 aeson servant-server wai text warp mtl transformers parsec

RUN mkdir -p reversible-debugger
WORKDIR /reversible-debugger

COPY stack.yaml reversible-debugger.cabal /reversible-debugger/
RUN stack setup && stack install --resolver lts-9.9 --dependencies-only 


# install elm
RUN npm install -g n && n 4.0.0
RUN npm install -g elm@0.18.0 

RUN mkdir -p /reversible-debugger/frontend
WORKDIR /reversible-debugger/frontend

COPY frontend/elm-package.json /reversible-debugger/frontend/
RUN elm-package install -y 

COPY . /reversible-debugger

RUN elm-make src/Main.elm src/ThreeBuyer.elm --output=index.html

WORKDIR /reversible-debugger
RUN stack build reversible-debugger:server


FROM fpco/stack-build:lts-9.9 

EXPOSE 8000
# CMD [ "stack", "exec", "server", "--", "$PORT" ] 

ENV LANG en_US.UTF-8

RUN mkdir -p /app
RUN mkdir -p /app/frontend
WORKDIR /app

# Executable(s) from build stage

COPY --from=0 /reversible-debugger/.stack-work/install/x86_64-linux/lts-9.9/8.0.2/bin/server /app/server
COPY --from=0 /reversible-debugger/frontend/index.html /app/frontend/index.html

RUN useradd app
USER app

# Reset fpco/stack-run's dumb ENTRYPOINT
# ENTRYPOINT []
CMD ["/app/server" ]
