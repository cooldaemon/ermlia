%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module is mock node pipe.

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

-module(ermlia_mock_node_pipe).

-export([setup/0, cleanup/0]).
-export([ping/3, find_node/4, find_value/4, put/6]).

-define(REAL_MODULE, ermlia_node_pipe).
-define(IP, {127, 0, 0, 1}).
-define(PORT, 20000).

setup() ->
  {ok, Mock} = smerl:for_module(?MODULE),
  smerl:compile(smerl:set_module(Mock, ?REAL_MODULE)).

cleanup() ->
  code:purge(?REAL_MODULE),
  code:load_file(?REAL_MODULE).

ping(_IP, _Port, _ID) ->
  ok.

find_node(_IP, Port, _ID, _TargetID) when ?PORT =< Port ->
  BasePort = (Port - ?PORT) * 100,
  lists:map(
    fun (N) -> {N, ?IP, ?PORT + N, unknown} end,
    lists:seq(BasePort + 1, BasePort + 20)
  );
find_node(_IP, _Port, _ID, _TargetID) ->
  [].

find_value(_IP, _Port, _ID, quu) ->
  {value, quux};
find_value(_IP, Port, _ID, baz) ->
  {nodes, find_node(foo, Port, foo, foo)};
find_value(_IP, _Port, _ID, _Key) ->
  {value, bar}.

put(_IP, _Port, _ID, _Key, _Value, _TTL) ->
  ok.

