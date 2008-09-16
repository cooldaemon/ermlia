## ermlia
The ermlia is Erlang implementation of Kademlia.
 
### Requirements
* [mochiweb](http://code.google.com/p/mochiweb/ "mochiweb")
* [smerl](http://code.google.com/p/smerl/ "smerl")
* [erljob](http://github.com/cooldaemon/erljob/ "erljob")
* [udp_server](http://gist.github.com/1458 "udp_server")
* [list_utils](http://gist.github.com/7239 "list_utils")

### Building from Source
    % cd /path/to
    % git clone git://github.com/cooldaemon/ermlia.git
    % cd ./ermlia
    % git submodule init
    % git submodule update
    % make

### How to Use
Start the first node.

    % cd /path/to/ermlia
    % ./start.sh 10000

Start the second node and put a key/value on this node.

    % cd /path/to/ermlia
    % ./start.sh 10001
    1> ermlia:join({127, 0, 0, 1}, 10000).
    2> ermlia:put(key, value).

Get a value from the first node.

    1> ermlia:get(key).

