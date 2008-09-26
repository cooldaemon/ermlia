%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module is supervisor for the ETS servers.

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

-module(ermlia_ets_server_sup).
-behaviour(supervisor).

-export([start_link/1, stop/1]).
-export([cleaner/1]).
-export([dump/1]).
-export([init/1]).

start_link(Module) ->
  add_job(Module, sup_utils:start_link(sup_name(Module))).

add_job(Module, {ok, _Pid}=Result) ->
  case erljob:add_job(
    cleaner_name(Module),
    {?MODULE, cleaner}, Module, 60000, infinity
  ) of
    ok ->
      Result;
    Other ->
      stop(Module),
      Other
  end;
add_job(_Module, Result) -> Result.

cleaner(Module) ->
  lists:foreach(fun (I) -> Module:clean(I) end, ilist()),
  Module.

dump(Module) ->
  lists:map(fun (I) -> {I, Module:dump(I)} end, ilist()).

stop(Module) ->
  erljob:delete_job(cleaner_name(Module)),
  sup_utils:stop(sup_name(Module)).

init([Module]) ->
  sup_utils:spec(
    lists:map(fun (I) ->
      {
        worker,
        atom_to_list(Module) ++ "_" ++ integer_to_list(I),
        permanent,
        Module,
        [I]
      }
    end, ilist())
  ).

ilist() ->
  lists:seq(0, 159).

sup_name(Module) ->
  list_utils:concat_atom([Module, "_sup"]).

cleaner_name(Module) ->
  list_utils:concat_atom([Module, "_cleaner"]).

