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
-export([i/1]).
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

id() ->
  gen_server:call(?MODULE, id).

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

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

key_to_id(Key) ->
 <<ID:160>> = crypto:sha(term_to_binary(Key)),
 ID.

i(Key) ->
  i(id(), key_to_id(Key)).

i(ID, KeyID) ->
  i(ID bxor KeyID, 1 bsl 159, 159).

i(_Digit, _Mask, -1) ->
  -1;
i(Digit, Mask, Interval) when Digit band Mask > 0 ->
  Interval;
i(Digit, Mask, Interval) ->
  i(Digit, Mask bsr 1, Interval - 1).

