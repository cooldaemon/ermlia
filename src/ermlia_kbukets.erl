%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc kbukets is Node routing table.
%%
%%  In the Kademlia literature, the routing table is referred to as k-buckets.

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

-module(ermlia_kbukets).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([add/4, lookup/1]).
-export([debug/1]).
-export([pong/2, ping_timeout/1]).
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

-define(LIST_MAX_LENGTH, 20).
-define(PING_TIMEOUT, 3000000).

start_link(I) ->
  gen_server:start_link({local, i_to_name(I)}, ?MODULE, [I], []).

stop(ServerRef) ->
  gen_server:call(ServerRef, stop).

add(I, ID, IP, Port) ->
  gen_server:cast(i_to_name(I), {add, {ID, IP, Port}}).

lookup(I) ->
  gen_server:call(i_to_name(I), lookup).

debug(I) ->
  gen_server:call(i_to_name(I), debug).

pong(ServerRef, SessionKey) ->
  gen_server:cast(ServerRef, {pong, SessionKey}).

ping_timeout(I) ->
  gen_server:cast(i_to_name(I), ping_timeout).

i_to_name(I) ->
  list_to_atom(atom_to_list(?MODULE) ++ "_" ++ integer_to_list(I)).

init([I]) ->
  process_flag(trap_exit, true),
  {ok, {[], []}}.

handle_call(lookup, _From, {Nodes, _AddNodes}=State) ->
  {reply, Nodes, State};

handle_call(debug, _From, {_Nodes, AddNodes}=State) ->
  {reply, AddNodes, State};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(_Message, _From, State) ->
  {reply, ok, State}.

handle_cast(
  {add, {ID, _IP, _Port}=AddNode},
  {Nodes, AddNodes}=State
) ->
  {noreply, add_node(
    lists:keymember(ID, 1, Nodes),
    length(Nodes ++ AddNodes),
    AddNode,
    State
  )};

handle_cast({pong, SessionKey}, {_Nodes, AddNodes}=State) ->
  {noreply, add_node_from_adding_list(
    lists:keysearch(SessionKey, 1, AddNodes),
    State
  )};

handle_cast(ping_timeout, {Nodes, AddNodes}) ->
  {noreply, check_ping_timeout(
    now_ms() - ?PING_TIMEOUT,
    Nodes,
    AddNodes,
    []
  )};

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

add_node(
  true,
  Length,
  {ID, _IP, _Port}=AddNode,
  {Nodes, AddNodes}
) ->
  add_node(false, Length, AddNode, {lists:keydelete(ID, 1, Nodes), AddNodes});
add_node(
  _NoExist,
  Length,
  AddNode,
  {[{_ID, IP, Port, _RTT}=Node | NodeTails], AddNodes}
) when ?LIST_MAX_LENGTH =< Length ->
  SessionKey = make_ref(),
  ermlia_node_pipe:ping({self(), SessionKey}, IP, Port),
  {
    NodeTails,
    [{SessionKey, AddNode, Node, now_ms()} | AddNodes]
  };
add_node(
  _NoExist,
  _Length,
  {ID, IP, Port},
  {Nodes, AddNodes}
) ->
  {lists:append(Nodes, [{ID, IP, Port, unknown}]), AddNodes}.

add_node_from_adding_list(false, State) ->
  State;
add_node_from_adding_list(
  {SessionKey, _AddNode, {ID, IP, Port, _RTT}, PingTime},
  {Nodes, AddNodes}
) ->
  {
    lists:append(Nodes, [{ID, IP, Port, now_ms() - PingTime}]),
    lists:keydelete(SessionKey, 1, AddNodes)
  }.

check_ping_timeout(_TimeOut, Nodes, [], NewAddNodes) ->
  {Nodes, NewAddNodes};
check_ping_timeout(
  TimeOut,
  Nodes,
  [{_SessionKey, {ID, IP, Port}, _Node, PingTime} | AddNodes],
  NewAddNodes
) when PingTime < TimeOut ->
  check_ping_timeout(
    TimeOut,
    lists:append(Nodes, [{ID, IP, Port, unknown}]),
    AddNodes,
    NewAddNodes
  );
check_ping_timeout(TimeOut, Nodes, [AddNode | AddNodes], NewAddNodes) ->
  check_ping_timeout(TimeOut, Nodes, AddNodes, [AddNode | NewAddNodes]).

now_ms() ->
  {MegaSecs, Secs, MicroSecs} = now(),
  MegaSecs * 1000000000000 + Secs * 1000000 + MicroSecs.

