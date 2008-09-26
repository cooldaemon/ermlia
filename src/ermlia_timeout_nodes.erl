%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module is a store for timeout nodes.

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

-module(ermlia_timeout_nodes).

-export([start_link/1, stop/1]).
-export([put/4, get/2]).
-export([dump/1]).
-export([clean/1]).

start_link(I) ->
  ermlia_ets_server:start_link(?MODULE, I).

stop(ServerRef) ->
  ermlia_ets_server:stop(ServerRef).

put(I, Key, Value, TTL) ->
  ermlia_ets_server:put(?MODULE, I, Key, Value, TTL).

get(I, Key) ->
  ermlia_ets_server:get(?MODULE, I, Key).

dump(I) ->
  ermlia_ets_server:dump(?MODULE, I).

clean(I) ->
  ermlia_ets_server:clean(?MODULE, I).
  
