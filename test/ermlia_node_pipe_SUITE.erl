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

-define(IP, {127, 0, 0, 1}).
-define(PORT, 10000).
-define(MOCK_PORT, 10001).

all() -> [testcase1].

init_per_testcase(_TestCase, Config) ->
  ermlia_mock_facade:setup(),
  ermlia_mock_node:start_link(?MOCK_PORT),
  ermlia_node_pipe:start_link(?PORT),
  Config.

end_per_testcase(_TestCase, _Config) ->
  ermlia_node_pipe:stop(),
  ermlia_mock_node:stop(),
  ermlia_mock_facade:cleanup(),
  ok.

testcase1() -> [].
testcase1(_Conf) ->
  ?assertMatch(ermlia_node_pipe:ping(?IP, ?MOCK_PORT, 1), ok, case1),
  ?assertMatch(ermlia_node_pipe:ping(?IP, 10002, 1), fail, case2),

  ?assertMatch(
    ermlia_node_pipe:find_node(?IP, ?MOCK_PORT, 1, 2),
    [],
    case3
  ),

  ?assertMatch(
    ermlia_node_pipe:find_value(?IP, ?MOCK_PORT, 1, foo),
    {value, bar},
    case4
  ),

  ?assertMatch(
    ermlia_node_pipe:put(?IP, ?MOCK_PORT, 1, foo, bar, 0),
    ok,
    case5
  ),

  ?assertMatch(send_and_recv({ping, 1}), {pong, _Pid}, case6),

  ?assertMatch(
    send_and_recv({find_node, 1, 1}),
    {
      ack_find_node, 
      [
        {1, {127, 0, 0, 1}, 10001, unknown},
        {2, {127, 0, 0, 1}, 10002, unknown}
      ],
      _Pid
    },
    case7
  ),

  ?assertMatch(
    send_and_recv({find_value, 1, foo}),
    {ack_find_value, {value, foo_value}, _Pid},
    case8
  ),

  ?assertMatch(send_and_recv({put, 1, foo, bar, 0}), fail, case9),

  ok.

send_and_recv({put, _ID, _Key, _Value, _TTL}=Message) ->
  udp_send_and_recv(Message);
send_and_recv(Message) ->
  udp_send_and_recv(erlang:append_element(Message, self())).

udp_send_and_recv(Message) ->
  {ok, Socket} = gen_udp:open(0, [binary]),
  ok = gen_udp:send(Socket, ?IP, ?PORT, term_to_binary(Message)),
  Result = receive
    {udp, Socket, _IP, _Port, Packet} ->
      binary_to_term(Packet)
  after 1000 ->
    fail
  end,
  gen_udp:close(Socket),
  Result.

