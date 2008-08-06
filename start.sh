erl -boot start_sasl -config ./ebin/elog +W w \
    -pa ./ebin \
    -pa ./deps/mochiweb/ebin \
    -pa ./deps/erljob/ebin \
    -s ermlia start 10000
