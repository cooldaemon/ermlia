%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module is data store manager.
%%
%%  This module is facade for k-buckets and data store.

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

-module(ermlia_facade).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([set_id/1, dump/0]).         % for test
-export([publish/3, get/1, join/2]). % for local
-export([                            % for remote
  add_node/3,
  find_node/1, find_value/1,
  put/3, get_appropriate_data/1, id/0
]). 
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

-define(FIND_VALUE_MAX_COUNT, 45).
-define(FIND_VALUE_PARALLEL_COUNT, 3).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

set_id(ID) ->
  erlang:put(ermlia_facade_id, ID),
  gen_server:cast(?MODULE, {set_id, ID}).

dump() ->
  [
    {id, id()},
    {data, ermlia_data_store_sup:dump()},
    {kbukets, ermlia_kbukets:dump()},
    {timeout, ermlia_timeout_nodes_sup:dump()}
  ].

publish(Key, Value, TTL) ->
  put(Key, Value, TTL),
  publish(find_node(key_to_id(Key)), Key, Value, TTL).

publish([], _Key, _Value, _TTL) ->
  ok;
publish([{_ID, IP, Port, _RTT} | Nodes], Key, Value, TTL) ->
  ermlia_node_pipe:put(IP, Port, id(), Key, Value, TTL),
  publish(Nodes, Key, Value, TTL).

get(Key) ->
  get(find_value(Key), Key).

get({value, Value}, _Key) ->
  Value;
get(Result, Key) ->
  KeyID = key_to_id(Key),
  get(Key, KeyID, [], [], Result).

get(_Key, _KeyID, _Nodes, _TermNodes, {value, Value}) ->
  Value;
get(_Key, _KeyID, _Nodes, TermNodes, _Result)
  when ?FIND_VALUE_MAX_COUNT < length(TermNodes)
->
  undefined;
get(Key, KeyID, Nodes, TermNodes, {nodes, AddNodes}) ->
  get(
    Key, KeyID, TermNodes, 
    merge_nodes(KeyID, Nodes, AddNodes, TermNodes)
  ).

get(_Key, _KeyID, _TermNodes, Nodes) when length(Nodes) =:= 0 ->
  undefined;
get(Key, KeyID, TermNodes, Nodes) ->
  {NewTermNodes, NewNodes} = list_utils:split(
    ?FIND_VALUE_PARALLEL_COUNT,
    Nodes
  ),
  MyID = id(),
  get(
    Key, KeyID,
    NewNodes, NewTermNodes ++ TermNodes,
    concat_find_value_results(
      list_utils:pmap(
        fun ({_ID, IP, Port, _RTT}=Node) ->
          {Node, ermlia_node_pipe:find_value(IP, Port, MyID, Key)}
        end,
        NewTermNodes
      )
    )
  ).

merge_nodes(KeyID, Nodes, AddNodes, TermNodes) ->
  filter_nodes(
    lists:sort(
      fun ({IDA, _IPA, _PortA, _RTTA}, {IDB, _IPB, _PortB, _RTTB}) ->
        [IA, IB] = lists:map(fun (ID) -> i(ID, KeyID) end, [IDA, IDB]),
        if
          IA < IB -> true;
          true    -> false
        end
      end,
      lists:usort(
        fun ({IDA, _IPA, _PortA, _RTTA}, {IDB, _IPB, _PortB, _RTTB}) ->
          if
            IDA =< IDB -> true;
            true       -> false
          end
        end,
        Nodes ++ AddNodes
      )
    ),
    TermNodes
  ).

filter_nodes(SortedNodes, []) ->
  SortedNodes;
filter_nodes(SortedNodes, TermNodes) ->
  list_utils:filter(
    [
      fun ({ID, _IP, _Port, _RTT}) ->
        MyID = id(),
        if
          ID =:= MyID -> false;
          true        -> true
        end
      end,
      fun ({ID, _IP, _Port, _RTT}) ->
        lists:any(
          fun ({IDT, _IPT, _PortT, _RTTT}) ->
            if
              ID =:= IDT -> false;
              true       -> true
            end
          end,
          TermNodes
        )
      end,
      fun ({ID, _IP, _Port, _RTT}) ->
        case ermlia_timeout_nodes:get(i(ID), ID) of
          undefined -> true;
          _Node     -> false
        end
      end
    ],
    SortedNodes
  ).

concat_find_value_results(Results) ->
  concat_find_value_results(Results, []).
  
concat_find_value_results([], Nodes) ->
  {nodes, Nodes};
concat_find_value_results([{_Node, {value, Value}} | _Results], _Nodes) ->
  {value, Value};
concat_find_value_results([{_Node, {nodes, NewNodes}} | Results], Nodes) ->
  concat_find_value_results(Results, Nodes ++ NewNodes);
concat_find_value_results([{Node, fail} | Results], Nodes) ->
  fail_node(Node),
  concat_find_value_results(Results, Nodes);
concat_find_value_results([_Other | Results], Nodes) ->
  concat_find_value_results(Results, Nodes).

fail_node({ID, _IP, _Port, _RTT}=Node) ->
  I = i(ID),
  ermlia_kbukets:delete(I, ID),
  ermlia_timeout_nodes:put(I, ID, Node).

join(IP, Port) ->
  find_and_add_nodes(IP, Port, ermlia_node_pipe:ping(IP, Port, id())).

find_and_add_nodes(_IP, _Port, fail) ->
  fail;
find_and_add_nodes(IP, Port, ID) ->
  add_node(ID, IP, Port),
  Nodes = ermlia_node_pipe:find_node(IP, Port, id(), id()),
  add_nodes(Nodes),
  find_nodes(Nodes).

add_nodes(Node, fail) ->
  fail_node(Node);
add_nodes(_Node, Nodes) ->
  add_nodes(Nodes).

add_nodes(fail) ->
  fail;
add_nodes([]) ->
  ok;
add_nodes([{ID, IP, Port, _RTT} | Nodes]) ->
  add_node(ID, IP, Port),
  add_nodes(Nodes).

find_nodes(fail) -> fail;
find_nodes(Nodes) ->
  MyID = id(),
  list_utils:pmap(
    fun ({_ID, IP, Port, _RTT}=Node) ->
      add_nodes(Node, ermlia_node_pipe:find_node(IP, Port, MyID, MyID))
    end,
    Nodes
  ),
  ok.

add_node(ID, IP, Port) ->
  I = i(ID),
  ping_pong(I, ermlia_kbukets:add(I, ID, IP, Port)).

ping_pong(I, {buckets_is_full, SessionKey, {_ID, IP, Port, _RTT}=Node}) ->
  pong(I, SessionKey, Node, ping(IP, Port));
ping_pong(_I, _Other) ->
  ok.

ping(IP, Port) ->
  ermlia_node_pipe:ping(IP, Port, id()).

pong(_I, _SessionKey, Node, fail) ->
  fail_node(Node),
  ok;
pong(I, SessionKey, _Node, _ID) ->
  ermlia_kbukets:pong(I, SessionKey).

find_node(ID) ->
  ermlia_kbukets:lookup(i(ID)).

find_value(Key) ->
  find_value(ermlia_data_store:get(key_to_i(Key), Key), Key).

find_value(undefined, Key) ->
  {nodes, find_node(key_to_id(Key))};
find_value(Value, _Key) ->
  {value, Value}.

put(Key, Value, TTL) ->
  ermlia_data_store:put(key_to_i(Key), Key, Value, TTL).

get_appropriate_data(ID) ->
  filter_data(ID, i(ID), ermlia_data_store_sup:dump(), []).

filter_data(_ID, _I, [], AppropriateData) ->
  lists:map(
    fun ({Key, {Value, TTL}}) -> {Key, Value, TTL} end,
    AppropriateData
  );
filter_data(ID, I, [{DataI, Data} | Dump], AppropriateData)
  when I < DataI
->
  filter_data(ID, I, Dump, AppropriateData ++ Data);
filter_data(ID, I, [{_DataI, []} | Dump], AppropriateData) ->
  filter_data(ID, I, Dump, AppropriateData);
filter_data(ID, I, [{DataI, Data} | Dump], AppropriateData) ->
  filter_data(
    ID, I, Dump,
    AppropriateData ++ lists:filter(
      fun ({Key, {_Value, _TTL}}) ->
        TI = i(ID, key_to_id(Key)),
        if
          DataI < TI -> false;
          true       -> true
        end
      end,
      Data
    )
  ).

id() ->
  id_cache(erlang:get(ermlia_facade_id)).

id_cache(undefined) ->
  ID = gen_server:call(?MODULE, id),
  erlang:put(ermlia_facade_id, ID),
  ID;
id_cache(ID) ->
  ID.

key_to_i(Key) ->
  i(key_to_id(Key)).

i(TargetID) ->
  i(id(), TargetID).

i(ID, TargetID) ->
  i(ID bxor TargetID, 1 bsl 159, 159).

i(_Digit, _Mask, -1) ->
  -1;
i(Digit, Mask, Interval) when Digit band Mask > 0 ->
  Interval;
i(Digit, Mask, Interval) ->
  i(Digit, Mask bsr 1, Interval - 1).

key_to_id(Key) ->
  <<ID:160>> = crypto:sha(term_to_binary(Key)),
  ID.

init(_Args) ->
  process_flag(trap_exit, true),
  ok = crypto:start(),
  <<ID:160>> = crypto:rand_bytes(20),
  {ok, {ID}}.

handle_call(id, _From, {ID}=State) ->
  {reply, ID, State};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(_Message, _From, State) ->
  {reply, ok, State}.

handle_cast({set_id, ID}, _State) ->
  {noreply, {ID}};

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

