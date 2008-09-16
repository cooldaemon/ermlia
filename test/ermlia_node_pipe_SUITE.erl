%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module is test suite for node pipe.

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

-module(ermlia_node_pipe_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("ermlia_test.hrl").

-define(MOCK_IP, {127, 0, 0, 1}).
-define(MOCK_PORT, 10001).

all() -> [testcase1].

init_per_testcase(_TestCase, Config) ->
  ermlia_mock_facade:setup(),
  ermlia_mock_node:start_link(?MOCK_PORT),
  ermlia_node_pipe:start_link(10000),
  Config.

end_per_testcase(_TestCase, _Config) ->
  ermlia_node_pipe:stop(),
  ok.

testcase1() -> [].
testcase1(_Conf) ->
  ?assertMatch(ermlia_node_pipe:ping(?MOCK_IP, ?MOCK_PORT, 1), ok, case1),
  ?assertMatch(ermlia_node_pipe:ping(?MOCK_IP, 10002, 1), fail, case2),

  ?assertMatch(
    ermlia_node_pipe:find_node(?MOCK_IP, ?MOCK_PORT, 1, 2),
    [],
    case3
  ),

  ?assertMatch(
    ermlia_node_pipe:find_value(?MOCK_IP, ?MOCK_PORT, 1, foo),
    {value, bar},
    case4
  ),

  ok = ermlia_node_pipe:put(?MOCK_IP, ?MOCK_PORT, 1, foo, bar, 0),

  ok.

