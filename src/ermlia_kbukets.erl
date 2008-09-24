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
-export([add/4, lookup/1, dump/0]).
-export([pong/2, ping_timeout/1]).
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

-define(LIST_MAX_LENGTH, 20).
-define(LOOKUP_MAX_LENGTH, 20).
-define(PING_TIMEOUT, 3000000).

start_link(I) ->
  gen_server:start_link({local, i_to_name(I)}, ?MODULE, [], []).

stop(ServerRef) ->
  gen_server:call(ServerRef, stop).

add(I, _ID, _IP, _Port) when I < 0; 159 < I ->
  ok;
add(I, ID, IP, Port) ->
  gen_server:call(i_to_name(I), {add, {ID, IP, Port}}).

lookup(I) when I < 0; 159 < I ->
  [];
lookup(I) ->
  lookup(I, 1, get_nodes(I)).

lookup(_I, Inc, Nodes) when
  ?LOOKUP_MAX_LENGTH =< length(Nodes);
  159 < Inc
->
  lists:sublist(Nodes, ?LOOKUP_MAX_LENGTH);
lookup(I, Inc, Nodes) ->
  lookup(I, Inc + 1, Nodes ++ get_nodes(I - Inc) ++ get_nodes(I + Inc)).

get_nodes(I) when I < 0; 159 < I ->
  [];
get_nodes(I) ->
  gen_server:call(i_to_name(I), get_nodes).

dump() ->
  lists:map(
    fun (I) -> {I, gen_server:call(i_to_name(I), dump)} end,
    lists:seq(0, 159)
  ).

pong(I, SessionKey) ->
  gen_server:cast(i_to_name(I), {pong, SessionKey}).

ping_timeout(I) ->
  gen_server:cast(i_to_name(I), ping_timeout).

i_to_name(I) ->
  list_utils:concat_atom([?MODULE, "_", I]).

init(_Args) ->
  process_flag(trap_exit, true),
  {ok, {[], []}}.

handle_call(get_nodes, _From, {Nodes, _AddNodes}=State) ->
  {reply, Nodes, State};

handle_call(dump, _From, State) ->
  {reply, State, State};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(
  {add, {ID, _IP, _Port}=AddNode},
  _From,
  {Nodes, AddNodes}=State
) ->
  add_node(
    lists:keymember(ID, 1, Nodes),
    length(Nodes ++ AddNodes),
    AddNode,
    State
  );

handle_call(_Message, _From, State) ->
  {reply, ok, State}.

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
  {[Node | NodeTails], AddNodes}
) when ?LIST_MAX_LENGTH =< Length ->
  SessionKey = make_ref(),
  {
    reply,
    {buckets_is_full, SessionKey, Node},
    {NodeTails, [{SessionKey, AddNode, Node, now_ms()} | AddNodes]}
  };
add_node(
  _NoExist,
  _Length,
  {ID, IP, Port},
  {Nodes, AddNodes}
) ->
  {reply, ok, {lists:append(Nodes, [{ID, IP, Port, unknown}]), AddNodes}}.

add_node_from_adding_list(false, State) ->
  State;
add_node_from_adding_list(
  {value, {SessionKey, _AddNode, {ID, IP, Port, _RTT}, PingTime}},
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

