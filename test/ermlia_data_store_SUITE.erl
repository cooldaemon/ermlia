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

-module(ermlia_data_store_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("ermlia_test.hrl").

all() -> [testcase1].

init_per_testcase(_TestCase, Config) ->
  erljob:start(),
  ermlia_data_store_sup:start_link(),
  erljob:set_interval(ermlia_data_store_cleaner, 1),
  Config.

end_per_testcase(_TestCase, _Config) ->
  ermlia_data_store_sup:stop(),
  erljob:stop(),
  ok.

testcase1() -> [].
testcase1(_Conf) ->
  ?assertEqual(ermlia_data_store:get(0, foo), undefined, case1),

  ermlia_data_store:put(0, foo, bar, 0),
  ?assertEqual(ermlia_data_store:get(0, foo), bar, case2),

  ermlia_data_store:put(0, foo, baz, 1),
  ?assertEqual(ermlia_data_store:get(0, foo), baz, case3),
  timer:sleep(1100),
  ?assertEqual(ermlia_data_store:get(0, foo), undefined, case4),

  ermlia_data_store:put(1, foo, quu, 0),
  ?assertEqual(ermlia_data_store:get(0, foo), undefined, case5),
  ?assertEqual(ermlia_data_store:get(1, foo), quu, case6),

  [DumpHead | DumpTails] = ermlia_data_store_sup:dump(),
  ?assertMatch(DumpHead, {0, [{foo, {baz, _TTL}}]}, case7),
  ?assertEqual(
    DumpTails,
       [{1, [{foo, {quu, 0}}]}]
    ++ lists:map(fun (N) -> {N, []} end, lists:seq(2, 159)),
    case8
  ),

  ok.

