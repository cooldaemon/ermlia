%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc application

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

-module(ermlia_app).
-behaviour(application).

-export([start/2, stop/1]). 

%% @equiv ermlia_sup:start_link([Port:integer()])
start(_Type, _Args) ->
  {ok, Port} = application:get_env(ermlia, port),
  ermlia_sup:start_link([Port]).

%% @doc Stop application for ermlia.
%% @spec stop(_State::term()) -> ok
stop(_State) -> ok.

