%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module is mock node. 

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

-module(ermlia_mock_node).
-behaviour(udp_server).

-include("udp_server.hrl").

-export([start_link/1, stop/0]).
-export([handle_call/4]).

start_link(Port) ->
  udp_server:receiver_start_link(
    {local, ?MODULE},
    ?MODULE,
    #udp_server_option{port=Port}
  ).

stop() ->
  case whereis(?MODULE) of
    Pid when is_pid(Pid) ->
      exit(Pid, normal),
      ok;
    _Other ->
      not_started
  end.

handle_call(Socket, IP, Port, Packet) ->
  dispatch(Socket, IP, Port, binary_to_term(Packet)),
  ok.

dispatch(Socket, IP, Port, {ping, _ID, Pid}) ->
  callback(Socket, IP, Port, {pong, 1, Pid});

dispatch(Socket, IP, Port, {find_node, _ID, _TargetID, Pid}) ->
  callback(Socket, IP, Port, {ack_find_node, [], Pid});

dispatch(Socket, IP, Port, {find_value, _ID, _Key, Pid}) ->
  callback(Socket, IP, Port, {ack_find_value, {value, bar}, Pid});

dispatch(_Socket, _IP, _Port, _Message) -> ok.

callback(Socket, IP, Port, Message) ->
  gen_udp:send(Socket, IP, Port, term_to_binary(Message)).

