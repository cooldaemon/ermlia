%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module is ETS manager server.

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

-module(ermlia_ets_server).
-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([put/5, get/3]).
-export([dump/2]).
-export([clean/2]).
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

start_link(Module, I) ->
  gen_server:start_link(
    {local, i_to_name(Module, I)},
    ?MODULE, [Module, I], []
  ).

stop(ServerRef) ->
  gen_server:call(ServerRef, stop).

put(Module, I, Key, Value, TTL) ->
  gen_server:cast(i_to_name(Module, I), {put, Key, Value, TTL}).

get(Module, I, Key) ->
  gen_server:call(i_to_name(Module, I), {get, Key}).

dump(Module, I) ->
  gen_server:call(i_to_name(Module, I), dump).

clean(Module, I) ->
  gen_server:cast(i_to_name(Module, I), clean).

i_to_name(Module, I) ->
  list_utils:concat_atom([Module, "_", I]).

init([Module, I]) ->
  process_flag(trap_exit, true),
  {ok, {ets:new(i_to_name(Module, I), [bag, private])}}.

 
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

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

microsecs() ->
  {MegaSecs, Secs, MicroSecs} = now(),
  MegaSecs * 1000000000000 + Secs * 1000000 + MicroSecs.

convert_ttl(0) ->
  0;
convert_ttl(TTL) ->
  microsecs() + TTL * 1000000.

lookup([]) ->
  undefined;
lookup([{_Key, {Value, 0}}]) ->
  Value;
lookup([{_Key, {Value, TTL}}]) ->
  lookup(Value, TTL, microsecs()).

lookup(Value, TTL, Now) when TTL > Now ->
  Value;
lookup(_Value, _TTL, _Now) ->
  undefined.

