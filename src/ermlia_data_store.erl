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

-export([start_link/0, stop/0]).
-export([set/2, set/3, get/1]).
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

%% @equiv gen_server:start_link({local, ?MODULE}, ?MODULE, [], [])
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @equiv gen_server:call(?MODULE, stop)
stop() -> gen_server:call(?MODULE, stop).

%% @equiv set(Key:atom(), Value:term(), 0)
set(Key, Value) -> set(Key, Value, 0).

%% @spec set(Key:atom(), Value:term(), TTL:integer()) -> ok
set(Key, Value, TTL) ->
  gen_server:cast(?MODULE, {set, Key, Value, TTL}).

%% @spec get(Key:atom()) -> Value:term()
get(Key) -> gen_server:call(?MODULE, {get, Key}).

%% @type tid()
%%  a table identifier, as returned by ets:new/2

%% @spec init(_Args:[]) -> {ok, {Ets:tid()}}
init(_Args) ->
  process_flag(trap_exit, true),
  {ok, {ets:new(data_store, [bag, private])}}.

%% @type form() = {pid(), Tag}.

%% @spec handle_call({get, Key:atom()}, _From:from(), {Ets:tid()}) ->
%%  {reply, Value:term(), State:term()}
handle_call({get, Key}, _From, State={Ets}) ->
  {reply, lookup(ets:lookup(Ets, Key)), State};

%% @spec handle_call(stop, _From:from(), State:term()) ->
%%  {stop, normal, stopped, State:term()}
handle_call(stop, _From, State) -> {stop, normal, stopped, State};

%% @spec handle_call(_Message:term(), _From:from(), State:term()) ->
%%  {reply, ok, State:term()}.
handle_call(_Message, _From, State) -> {reply, ok, State}.

%% @spec handle_cast(
%%    {set, Key:atom(), Value:term(), TTL:integer()}, {Ets:tid()}
%%  ) -> {noreply, State:term()}
handle_cast({set, Key, Value, TTL}, State={Ets}) ->
  ets:delete(Ets, Key),
  ets:insert(Ets, {Key, {Value, convert_ttl(TTL)}}),
  {noreply, State};

%% @spec handle_cast(_Message:term(), _State:term()) ->
%%  {noreply, State:term()}
handle_cast(_Message, State) -> {noreply, State}.

%% @spec handle_cast(_Info:term(), _State:term()) ->
%%  {noreply, State:term()}
handle_info(_Info, State) -> {noreply, State}.

%% @spec terminate(_Reason:term(), _State:term()) -> ok
terminate(_Reason, _State) -> ok.

%% @spec code_change(_Reason:term(), _State:term()) -> {ok, State:term()}
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

