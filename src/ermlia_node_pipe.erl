%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module is pipe that connects between nodes. 

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

-module(ermlia_node_pipe).
-behaviour(udp_server).

-export([ping/3]).
-export([handle_call/4, handle_info/2]).

ping(IP, Port, Callback) ->
  ermlia_node_pipe ! {ping, {IP, Port, Callback}}.

handle_call(Socket, IP, Port, Packet) ->
  dispatch(Socket, IP, Port, binary_to_term(Packet)).

handle_info(Socket, {ping, {IP, Port, Callback}}) ->
  gen_udp:send(Socket, IP, Port, term_to_binary({ping, Callback}));

handle_info(_Socket, _Message) -> ok.

dispatch(Socket, IP, Port, {ping, Callback}) ->
  gen_udp:send(Socket, IP, Port, term_to_binary({pong, Callback}));

dispatch(_Socket, _IP, _Port, {pong, {Pid, SessionKey}}) ->
  ermlia_kbukets:pong(Pid, SessionKey);

dispatch(_Socket, _IP, _Port, _Message) -> ok.

