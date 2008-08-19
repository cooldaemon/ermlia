%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module is supervisor for the k-bukets.

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

-module(ermlia_kbukets_sup).
-behaviour(supervisor).

-export([start_link/0, stop/0]).
-export([ping_timeout/1]).
-export([init/1]).

start_link() ->
  add_job(sup_utils:start_link(?MODULE)).

add_job({ok, Pid}=Result) ->
  case erljob:add_job(
    "ermlia_kbukets_ping_timeout",
    {?MODULE, ping_timeout}, {}, 1000, infinity
  ) of
    ok    -> Result;
    Other -> stop(), Other
  end;
add_job(Result) -> Result.

ping_timeout(State) ->
  lists:foreach(fun ermlia_kbukets:ping_timeout/1, lists:seq(0, 159)),
  State.

stop() ->
  sup_utils:stop(?MODULE).

init(_Args) ->
  sup_utils:spec(
    lists:map(fun (I) ->
      {
        worker,
        "ermlia_kbukets_" ++ integer_to_list(I),
        permanent,
        ermlia_kbukets,
        [I]
      }
    end, lists:seq(0, 159))
  ).

