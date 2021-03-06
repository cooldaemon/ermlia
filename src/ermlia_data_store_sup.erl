%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module is supervisor for the data stores.

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

-module(ermlia_data_store_sup).
-behaviour(supervisor).

-export([start_link/0, stop/0]).
-export([dump/0]).
-export([init/1]).

start_link() ->
  ermlia_ets_server_sup:start_link(ermlia_data_store).

stop() ->
  ermlia_ets_server_sup:stop(ermlia_data_store).

dump() ->
  ermlia_ets_server_sup:dump(ermlia_data_store).

init(_Args) ->
  ermlia_ets_server_sup:init([ermlia_data_store]).

