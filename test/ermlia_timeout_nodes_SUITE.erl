%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module is test suite for timeout nodes store.

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

-module(ermlia_timeout_nodes_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("ermlia_test.hrl").

all() -> [testcase1].

init_per_testcase(_TestCase, Config) ->
  erljob:start(),
  ermlia_timeout_nodes_sup:start_link(),
  Config.

end_per_testcase(_TestCase, _Config) ->
  ermlia_timeout_nodes_sup:stop(),
  erljob:stop(),
  ok.

testcase1() -> [].
testcase1(_Conf) ->
  ?assertEqual(ermlia_timeout_nodes:get(0, 1), undefined, case1),

  ermlia_timeout_nodes:put(0, 1, get_node(1)),
  ?assertEqual(ermlia_timeout_nodes:get(0, 1), get_node(1), case2),

  [DumpHead | _DumpTails] = ermlia_timeout_nodes_sup:dump(),
  ?assertMatch(
    DumpHead,
    {0, [{1, {{1, {127, 0, 0, 1}, 10001, unknown}, _TTL}}]},
    case3
  ),

  ok.

get_node(N) ->
  {N, {127, 0, 0, 1}, 10000 + N, unknown}.

