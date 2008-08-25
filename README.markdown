## ermlia
ermlia is Erlang implementation of Kademlia.
 
### Requirements
* [mochiweb](http://code.google.com/p/mochiweb/ "mochiweb")
* [erljob](http://github.com/cooldaemon/erljob/ "erljob")

### Building from Source
1. Run `git submodule init` & `git submodule update` in the project root directory.
2. Run `make` in the project root directory.

### How to Use
    % cd /path/to/project/root
    % ./start.sh 10000
    1> ermlia:join("127.0.0.1", 10001).
    2> ermlia:set(key, value).
    3> ermlia:get(key).

