# Build stage 0
FROM erlang:25-alpine
RUN apk update
RUN apk add git

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang application
COPY config game_server/config
COPY include game_server/include
COPY src game_server/src
COPY rebar.config game_server/rebar.config

# And build the release
WORKDIR game_server
RUN rebar3 eunit
RUN rebar3 ct
RUN rebar3 release

# Build stage 1
FROM erlang:25-alpine

# # Install the released application
COPY --from=0 /buildroot/game_server/_build/default/rel/game_server /game_server

# Expose relevant ports
EXPOSE 8080

CMD "/game_server/bin/game_server" "foreground"