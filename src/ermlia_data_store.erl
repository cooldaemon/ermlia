%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module is data store manager.
%%
%%  This module uses the ETS module internally.

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

-module(ermlia_data_store).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([put/3, put/4, get/2]).
-export([dump/0]).
-export([clean/1]).
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

start_link(I) ->
  gen_server:start_link({local, i_to_name(I)}, ?MODULE, [I], []).

stop(ServerRef) ->
  gen_server:call(ServerRef, stop).

put(I, Key, Value) ->
  put(I, Key, Value, 0).

put(I, Key, Value, TTL) ->
  gen_server:cast(i_to_name(I), {put, Key, Value, TTL}).

get(I, Key) ->
  gen_server:call(i_to_name(I), {get, Key}).

dump() ->
  lists:map(
    fun (I) -> {I, gen_server:call(i_to_name(I), dump)} end,
    lists:seq(0, 159)
  ).

i_to_name(I) ->
  list_utils:concat_atom([?MODULE, "_", I]).

init([I]) ->
  process_flag(trap_exit, true),
  {ok, {ets:new(i_to_name(I), [bag, private])}}.

clean(I) ->
  gen_server:cast(i_to_name(I), clean).
  
handle_call({get, Key}, _From, State={Ets}) ->
  {reply, lookup(ets:lookup(Ets, Key)), State};

handle_call(dump, _From, State={Ets}) ->
  {reply, ets:tab2list(Ets), State};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(_Message, _From, State) ->
  {reply, ok, State}.

handle_cast({put, Key, Value, TTL}, State={Ets}) ->
  ets:delete(Ets, Key),
  ets:insert(Ets, {Key, {Value, convert_ttl(TTL)}}),
  {noreply, State};

handle_cast(clean, State={Ets}) ->
  Now = microsecs(),
  ets:select_delete(Ets, [{
    {'$1', {'$2', '$3'}},
    [{'/=', '$3', 0}, {'<', '$3', Now}],
    ['$2']
  }]),
  {noreply, State};

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

microsecs() ->
  {MegaSecs, Secs, MicroSecs} = now(),
  MegaSecs * 1000000000000 + Secs * 1000000 + MicroSecs.

convert_ttl(0)   -> 0;
convert_ttl(TTL) -> microsecs() + TTL * 1000000.

lookup([])                     -> undefined;
lookup([{_Key, {Value, 0}}])   -> Value;
lookup([{_Key, {Value, TTL}}]) -> lookup(Value, TTL, microsecs()).

lookup(Value, TTL, Now) when TTL > Now -> Value;
lookup(_Value, _TTL, _Now)             -> undefined.

