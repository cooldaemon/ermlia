#!/bin/sh

if [ $# = 0 ]; then
  echo "usage: $0 [port]"
  exit 1
fi

erl -boot start_sasl -config ./ebin/elog +W w \
    -pa ./ebin \
    -pa ./deps/mochiweb/ebin \
    -pa ./deps/erljob/ebin \
    -eval "ermlia:start($1)"

