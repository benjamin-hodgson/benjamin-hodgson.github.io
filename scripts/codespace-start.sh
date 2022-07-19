#!/usr/bin/zsh

cabal run benjamin-pizza -- watch --no-server &> _logs/hakyll.log &!
live-server _site --port=8888 &> _logs/server.log &!
