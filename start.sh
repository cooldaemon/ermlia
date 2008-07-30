erl -boot start_sasl -config ./ebin/elog +W w \
    -pa ./ebin -pa ./deps/mochiweb/ebin \
    -s ermlia start 10000
