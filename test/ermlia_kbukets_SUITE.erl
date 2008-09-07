%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module is test suite for routing table manager.

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

-module(ermlia_kbukets_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("ermlia_test.hrl").

all() -> [testcase1].

init_per_testcase(_TestCase, Config) ->
  erljob:start(),
  ermlia_kbukets_sup:start_link(),
  Config.

end_per_testcase(_TestCase, _Config) ->
  ermlia_kbukets_sup:stop(),
  erljob:stop(),
  ok.

testcase1() -> [].
testcase1(_Conf) ->
  ?assertEqual(ermlia_kbukets:lookup(0), [], case1),

  ?assertEqual(add_node(1), ok, case2),
  ?assertEqual(ermlia_kbukets:lookup(0), [get_node(1)], case3),

  ?assertEqual(add_node(2), ok, case4),
  ?assertEqual(ermlia_kbukets:lookup(0), get_nodes(1, 2), case5),

  ?assertEqual(
    lists:map(fun (N) -> add_node(N, N) end, [4, 3]),
    cycle(ok, 2),
    case6
  ),

  ?assertEqual(add_node(5), ok, case7),
  ?assertEqual(
    ermlia_kbukets:lookup(0),
    get_nodes(1, 2) ++ [get_node(5)] ++ get_nodes(3, 4),
    case8
  ),

  ?assertEqual(add_nodes(6, 20), cycle(ok, 15), case9),
  ?assertEqual(
    ermlia_kbukets:lookup(0),
    get_nodes(1, 2) ++ get_nodes(5, 20) ++ get_nodes(3, 4),
    case10
  ),

  ?assertEqual(add_nodes(21, 22), cycle(ok, 2), case11),
  ?assertEqual(
    ermlia_kbukets:lookup(0),
    get_nodes(1, 2) ++ get_nodes(5, 22),
    case12
  ),

  {buckets_is_full, SessionKey1, Node1} = add_node(23),
  ?assertEqual(Node1, get_node(1), case13),
  ?assertEqual(
    ermlia_kbukets:lookup(0),
    [get_node(2)] ++ get_nodes(5, 22) ++ [get_node(3)],
    case14
  ),

  ermlia_kbukets:pong(0, SessionKey1),
  case lists:last(ermlia_kbukets:lookup(0)) of
    {1, {127, 0, 0, 1}, 10001, _RTT} ->
      ok;
    OtherNode -> 
      ct:fail({case15, OtherNode})
  end,

  {buckets_is_full, _SessionKey2, Node2} = add_node(23),
  ?assertEqual(Node2, get_node(2), case16),

  timer:sleep(4000),
  ?assertEqual(lists:last(ermlia_kbukets:lookup(0)), get_node(23), case17),

  ok.

get_nodes(H, T) ->
  lists:map(fun (N) -> get_node(N) end, lists:seq(H, T)).

get_node(N) ->
  {N, {127, 0, 0, 1}, 10000 + N, unknown}.

add_nodes(H, T) ->
  lists:map(fun (N) -> add_node(N) end, lists:seq(H, T)).

add_node(N) ->
  add_node(0, N).

add_node(I, N) ->
  ermlia_kbukets:add(I, N, {127, 0, 0, 1}, 10000 + N).

cycle(Elem, Count) ->
  lists:flatten(lists:map(fun (_N) -> Elem end, lists:seq(1, Count))).

