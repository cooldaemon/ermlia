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

-export([handle_call/4, handle_info/1]).

%% @spec handle_call(
%%  _Socket:term(), _Address:term(), _Port:term(), _Packet:term()) -> ok
handle_call(_Socket, _Address, _Port, _Packet) -> ok.

%% @spec handle_info(_Message:term()) -> ok
handle_info(_Message) -> ok.

