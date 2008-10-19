#!/bin/sh

ROOT=%ROOT_DIR%

if [ $# = 0 ]; then
  echo "usage: $0 [port]"
  exit 1
fi

erl -boot start_sasl -config ${ROOT}/ebin/elog +W w \
    -pa ${ROOT}/ebin \
    -pa ${ROOT}/deps/mochiweb/ebin \
    -pa ${ROOT}/deps/erljob/ebin \
    -eval "ermlia:start($1)"

