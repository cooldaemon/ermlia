%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc ermlia is easy key/value store.
%%
%%  ermlia is Erlang implementation of Kademlia.
%%
%%  Here's a quick example illustrating how to use ermlia: 
%%  ```
%%    ermlia:start(MyPort),
%%    ermlia:join(Host, Port),
%%    ermlia:set(Key, Value),
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
-export([set/2, get/1]). 

%% @equiv application:start(ermlia, permanent)
start(Port) ->
  application:set_env(ermlia, port, Port),
  application:start(ermlia, permanent).

%% @equiv application:stop(ermlia)
stop() -> application:stop(ermlia).

%% @equiv ermlia_facade:join(Host, Port)
join(Host, Port) -> ermlia_facade:join(Host, Port).

%% @equiv ermlia_facade:set(Key, Value)
set(Key, Value) -> ermlia_facade:set(Key, Value).

%% @equiv ermlia_facade:get(Key)
get(Key) -> ermlia_facade:get(Key).

