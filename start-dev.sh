#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname minecraft_dev \
    -s minecraft \
    -s reloader
