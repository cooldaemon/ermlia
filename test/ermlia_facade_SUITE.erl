%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module is test suite for data store manager.

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

-module(ermlia_facade_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("ermlia_test.hrl").

-define(IP, {127, 0, 0, 1}).
-define(PORT_FOR_REMOTE_TEST, 10000).
-define(PORT_FOR_LOCAL_TEST, 20000).

all() -> [for_remote, for_local].

init_per_testcase(_TestCase, Config) ->
  erljob:start(),
  ermlia_data_store_sup:start_link(),
  ermlia_kbukets_sup:start_link(),
  ermlia_timeout_nodes_sup:start_link(),
  ermlia_mock_node_pipe:setup(),
  ermlia_facade:start_link(),
  Config.

end_per_testcase(_TestCase, _Config) ->
  ermlia_facade:stop(),
  crypto:stop(),
  ermlia_mock_node_pipe:cleanup(),
  ermlia_timeout_nodes_sup:stop(),
  ermlia_kbukets_sup:stop(),
  ermlia_data_store_sup:stop(),
  erljob:stop(),
  ok.

for_remote() -> [].
for_remote(_Conf) ->
  ermlia_facade:set_id(1),

  ?assertEqual(add_nodes(32, 51), list_utils:cycle(ok, 20), case1),
  ?assertEqual(ermlia_facade:find_node(32), get_nodes(32, 51), case2),

  ?assertEqual(
    ermlia_facade:find_value(foo),
    {nodes, get_nodes(32, 51)},
    case3
  ),

  ?assertEqual(ermlia_facade:put(foo, bar, 0), ok, case4),
  ?assertEqual(ermlia_facade:find_value(foo), {value, bar}, case5),

  ?assertEqual(add_node(52), ok, case6),
  ?assertMatch(
    lists:last(ermlia_facade:find_node(32)),
    {32, ?IP, 10032, _RTT},
    case7
  ),

  <<IDfoo:160>>   = crypto:sha(term_to_binary(foo)),
  <<IDwaldo:160>> = crypto:sha(term_to_binary(waldo)),
  ermlia_facade:set_id(IDfoo),

  ?assertEqual(ermlia_facade:put(quux, "155 158", 0), ok, case8),
  ?assertEqual(ermlia_facade:put(bar,  "157 158", 0), ok, case9),
  ?assertEqual(ermlia_facade:put(fred, "158 157", 0), ok, case10),
  ?assertEqual(ermlia_facade:put(baz,  "159 159", 0), ok, case11),

  % 158 = i(IDfoo, IDwaldo).
  ?assertEqual(
    ermlia_facade:get_appropriate_data(IDwaldo),
    [
      {fred, "158 157", 0},
      {baz,  "159 159", 0},
      {foo,  bar,       0}
    ], case12
  ),

  ok.

for_local() -> [].
for_local(_Conf) ->
  ?assertMatch(ermlia_facade:join(?IP, ?PORT_FOR_LOCAL_TEST), ok, case1),

  ?assertMatch(ermlia_facade:publish(foo, bar, 0), ok, case2),
  ?assertMatch(ermlia_facade:get(foo), bar, case3),

  ?assertMatch(ermlia_facade:get(baz), undefined, case4),
  ?assertMatch(ermlia_facade:get(quu), quux, case5),

  ?assertMatch(
    ermlia_facade:dump(),
    [{id, _ID}, {data, _Data}, {kbukets, _Kbukets}, {timeout, _Timeout}],
    case6
  ),

  ok.

add_nodes(H, T) ->
  lists:map(fun (N) -> add_node(N) end, lists:seq(H, T)).

add_node(N) ->
  ermlia_facade:add_node(N, ?IP, ?PORT_FOR_REMOTE_TEST + N).

get_nodes(H, T) ->
  lists:map(fun (N) -> get_node(N) end, lists:seq(H, T)).

get_node(N) ->
  {N, ?IP, ?PORT_FOR_REMOTE_TEST + N, unknown}.

