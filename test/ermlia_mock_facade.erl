%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module is mock data store manager.
%%
%%  This module is facade mock for k-buckets and data store.

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

-module(ermlia_mock_facade).

-export([setup/0, cleanup/0]).
-export([
  add_node/3, find_node/1, find_value/1, put/3, get_appropriate_data/1, id/0
]).

-define(REAL_MODULE, ermlia_facade).


setup() ->
  {ok, Mock} = smerl:for_module(?MODULE),
  smerl:compile(smerl:set_module(Mock, ?REAL_MODULE)).

cleanup() ->
  code:purge(?REAL_MODULE),
  code:load_file(?REAL_MODULE).

add_node(_ID, _IP, _Port) ->
  ok.

find_node(1) ->
  [
    {1, {127, 0, 0, 1}, 10001, unknown},
    {2, {127, 0, 0, 1}, 10002, unknown}
  ];
find_node(_ID) ->
  [].

find_value(foo) ->
  {value, foo_value};
find_value(bar) ->
  {nodes, find_node(1)};
find_value(_Key) ->
  {nodes, []}.

put(_Key, _Value, _TTL) ->
  ok.

get_appropriate_data(_ID) ->
  [{foo, bar, 0}].

id() ->
  1.

