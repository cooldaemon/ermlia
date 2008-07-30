%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc UDP Server Behaviour.

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

-module(udp_server).
-behaviour(supervisor).

-include("udp_server.hrl").
 
-export([behaviour_info/1]).
 
-export([start_link/1, start_link/2, start_link/3]).
-export([stop/0, stop/1]).
-export([init/1]).
 
-export([receiver_start_link/3]).
-export([receiver_init/3]).
 
% Behaviour Callbacks
behaviour_info(callbacks) -> [{handle_call, 4}, {handle_info, 1}];
behaviour_info(_Other) -> undefined.
 
% Supervisor
%% External APIs
start_link(Module) ->
  start_link(Module, #udp_server_option{}).
 
start_link(RegistName, Module) when is_atom(Module) ->
  start_link(RegistName, Module, #udp_server_option{});
start_link(Module, Option) ->
  start_link({local, ?MODULE}, Module, Option).
 
start_link({Dest, Name}=RegistName, Module, Option) ->
  supervisor:start_link(
    {Dest, supervisor_name(Name)},
    ?MODULE,
    [RegistName, Module, Option]
  ).
 
stop() ->
  stop(?MODULE).
 
stop(Name) ->
  case whereis(supervisor_name(Name)) of
    Pid when is_pid(Pid) ->
      exit(Pid, normal),
      ok;
    _ -> not_started
  end.
 
supervisor_name(Name) ->
  list_to_atom(atom_to_list(Name) ++ "_sup").
 
%% Callbacks
init([{_Dest, Name}=RegistName, Module, Option]) ->
  #udp_server_option{
    max_restarts = MaxRestarts,
    time = Time,
    shutdown = Shutdown
  } = Option,
  {ok, {{one_for_one, MaxRestarts, Time}, [
    {
      Name,
      {?MODULE, receiver_start_link, [RegistName, Module, Option]},
      permanent,
      Shutdown,
      worker,
      []
    }
  ]}}.
 
% ProcLib - udp_server_receiver
%% External APIs
receiver_start_link({Dest, Name}, Module, Option) ->
  {ok, Pid}
    = proc_lib:start_link(?MODULE, receiver_init, [self(), Module, Option]),
  case Dest of
    local -> register(Name, Pid);
    _Global -> global:register_name(Name, Pid)
  end,
  {ok, Pid}.
 
%% Callbacks
receiver_init(Parent, Module, Option) ->
  case gen_udp:open(
    Option#udp_server_option.port,
    Option#udp_server_option.option
  ) of
    {ok, Socket} ->
      proc_lib:init_ack(Parent, {ok, self()}),
      recv(
        proplists:get_value(active, Option#udp_server_option.option),
        Socket, Module, Option
      );
    {error, Reason} ->
      exit({error, Reason})
  end.
 
recv(false, Socket, Module, Option) ->
  case gen_udp:recv(
    Socket,
    Option#udp_server_option.recv_length,
    Option#udp_server_option.recv_timeout
  ) of
    {ok, {Address, Port, Packet}} ->
      Module:handle_call(Socket, Address, Port, Packet),
      recv(false, Socket, Module, Option);
    {error, Reason} ->
      exit({error, Reason})
  end;
 
recv(_Active, Socket, Module, Option) ->
  receive
    {udp, Socket, Address, Port, Packet} ->
      Module:handle_call(Socket, Address, Port, Packet),
      recv(true, Socket, Module, Option);
    OtherMessage ->
      Module:handle_info(OtherMessage),
      recv(true, Socket, Module, Option)
  after Option#udp_server_option.recv_timeout ->
    exit({error, udp_timeout})
  end.
