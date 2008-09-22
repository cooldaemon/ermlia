%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc ermlia is easy key/value store.
%%
%%  ermlia is Erlang implementation of Kademlia.
%%
%%  Here's a quick example illustrating how to use ermlia: 
%%  ```
%%    ermlia:start(MyPort),
%%    ermlia:join(IP, Port),
%%    ermlia:put(Key, Value),
%%    ermlia:get(Key),
%%    ermlia:stop()
%%  '''

%% Copyright 2008 Masahito Ikuta
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(ermlia).

-export([start/1, stop/0]). 
-export([join/2]). 
-export([put/2, put/3, get/1]). 

start(Port) ->
  application:set_env(ermlia, port, Port),
  erljob:start(),
  application:start(ermlia, permanent).

stop() ->
  application:stop(ermlia),
  erljob:stop().

%% @equiv ermlia_facade:join(IP, Port)
join(IP, Port) ->
  ermlia_facade:join(IP, Port).

%% @equiv put(Key, Value, 0)
put(Key, Value) ->
  put(Key, Value, 0).

%% @equiv ermlia_facade:put(Key, Value, TTL)
put(Key, Value, TTL) ->
  ermlia_facade:put(Key, Value, TTL).

%% @equiv ermlia_facade:get(Key)
get(Key) ->
  ermlia_facade:get(Key).

