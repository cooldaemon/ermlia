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
  ermlia_data_store:start_link(),
  Config.

end_per_testcase(_TestCase, _Config) ->
  ermlia_data_store:stop(),
  ok.

testcase1() -> [].
testcase1(_Conf) ->
  ?assertEqual(ermlia_data_store:get(foo), undefined, case1),
  ermlia_data_store:set(foo, bar),
  ?assertEqual(ermlia_data_store:get(foo), bar, case2),
  ermlia_data_store:set(foo, baz, 3),
  ?assertEqual(ermlia_data_store:get(foo), baz, case3),
  timer:sleep(3000),
  ?assertEqual(ermlia_data_store:get(foo), undefined, case4),
  ok.

