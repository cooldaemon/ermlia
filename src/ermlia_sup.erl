%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc supervisor

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

-module(ermlia_sup).
-behaviour(supervisor).

-include("udp_server.hrl"). 

-export([start_link/1, stop/0]).
-export([init/1]).

-define(MAX_RESTART, 0).
-define(TIME, 1).
-define(SHUTDOWN_WAITING_TIME, 2000).

%% @equiv supervisor:start_link({local, ?MODULE}, ?MODULE, Args:[term()])
start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% @doc Stop supervisor for ermlia.
%% @spec stop() -> ok | not_started
stop() ->
  case whereis(?MODULE) of
    Pid when is_pid(Pid) ->
      exit(Pid, normal),
      ok;
    _Other ->
      not_started
  end.

%% @doc Callback for supervisor.
%% @spec init([Port:integer()]) -> {ok, {Option:term(), [ChildSpec:term()]}}
init([Port]) ->
  {ok, {{one_for_one, ?MAX_RESTART, ?TIME}, [
    worker_spec(ermlia_kbukets, []),
    worker_spec(ermlia_data_store, []),
    worker_spec(udp_server, receiver_start_link, [
      {local, ermlia_node_pipe},
      ermlia_node_pipe,
      #udp_server_option{port=Port}
    ]),
    worker_spec(mochiweb_http, start, [[
      {port, Port},
      {name, {local, ermlia_web}},
      {loop, {ermlia_web, dispatch}}
    ]])
  ]}}.

worker_spec(Module, Args) ->
  worker_spec(Module, start_link, Args).

worker_spec(Module, Function, Args) ->
  {
    Module,
    {Module, Function, Args},
    permanent,
    ?SHUTDOWN_WAITING_TIME,
    worker,
    [Module]
  }.

%supervisor_spec(Module, Args) ->
%  StartFunc = {Module, start_link, Args},
%  {Module, StartFunc, permanent, infinity, supervisor, []}.

