%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc Web server for ermlia.

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

-module(ermlia_web).

-export([start_link/1, stop/0]).
-export([loop/1]).

-define(VALUE_MAX_SIZE, 1024*1024*1024). % 1MByte

start_link(Port) ->
  mochiweb_http:start([
    {port, Port},
    {name, {local, ?MODULE}},
    {loop, {?MODULE, loop}}
  ]).

stop() ->
  mochiweb_http:stop(?MODULE).

loop(Req) ->
  dispatch(Req, Req:get(method), Req:get(path)).

dispatch(Req, _Method, "/") ->
  Dump = ermlia:dump(),
  ID = integer_to_list(proplists:get_value(id, Dump)),
  Data = proplists:get_value(data, Dump),
  Kbukets = proplists:get_value(kbukets, Dump),
  Timeout = proplists:get_value(timeout, Dump),
  ok(Req, <<"dump">>, tables([
    {
      <<"Status">>,
      [tr(th, [<<"Name">>, <<"Value">>])],
      [tr(td, [<<"ID">>, ID])]
    },
    {
      <<"Data">>,
      [tr(th, [<<"I">>, <<"Values">>])],
      trs(Data, fun trs_for_data/3, fun tds_for_data/1)
    },
    {
      <<"K-Bukets">>,
      [tr(th, [<<"I">>, <<"Nodes">>, <<"Add Nodes">>])],
      trs(Kbukets, fun trs_for_kbukets/3, fun tds_for_kbukets/1)
    },
    {
      <<"Timeout Nodes">>,
      [tr(th, [<<"I">>, <<"Values">>])],
      trs(Timeout, fun trs_for_data/3, fun tds_for_data/1)
    }
  ]));

dispatch(Req, _Method, "/css" ++ Path) ->
  Req:serve_file("css" ++ Path, docroot());

dispatch(Req, Method, "/" ++ Path)
  when Method =:= 'GET'; Method =:= 'HEAD'
->
  case ermlia:get(Path) of
    {Type, Body} -> Req:ok({Type, Body});
    _Other       -> Req:not_found()
  end;

dispatch(Req, 'PUT', "/" ++ Path) ->
  ermlia:put(
    Path,
    {
      Req:get_header_value("Content-Type"),
      Req:recv_body(?VALUE_MAX_SIZE)
    },
    list_to_integer(Req:get_header_value("x-ermlia-expire"))
  ),
  respond(Req, 202, "Accepted.");

dispatch(Req, _Method, _Path) ->
  respond(Req, 405, "Method not allowed.").

respond(Req, Code, Message) ->
  HResponse = mochiweb_headers:enter(
    "Content-Type",
    "text/plain",
    mochiweb_headers:empty()
  ),
  Req:respond({Code, HResponse, Message}).

ok(Req, Title, Body) ->
  Req:ok({
    "text/html",
    [{"Server", atom_to_list(?MODULE)}],
    mochiweb_html:to_html(
      {html, [], [
        {head, [], [
          {title, [], [<<"ermlia - ">>, Title]},
          {link, [
            {rel,   <<"stylesheet">>},
            {type,  <<"text/css">>},
            {href,  <<"/css/table.css">>},
            {media, <<"screen">>}
          ], []}
        ]},
        {body, [], Body}
      ]}
    )
  }).

tables(Lists) ->
  lists:map(
    fun ({Caption, TheadTRs, TbodyTRs}) ->
      table(Caption, TheadTRs, TbodyTRs)
    end,
    Lists
  ).

table(Caption, TheadTRs, TbodyTRs) ->
  {table, [], [
    {caption, [], [Caption]},
    {thead, [], TheadTRs},
    {tbody, [], TbodyTRs}
  ]}.

tr(ChildTag, Lists) ->
  {tr, [], lists:map(fun (Elem) -> {ChildTag, [], [Elem]} end, Lists)}.

trs(Lists, TRsFun, TDsFun) ->
  trs(Lists, TRsFun, TDsFun, []).

trs([], _TRsFun, _TDsFun, TRs) ->
  lists:reverse(TRs);
trs([Head | Tails], TRsFun, TDsFun, TRs) ->
  trs(
    Tails,
    TRsFun,
    TDsFun,
    TRsFun(Head, TDsFun, TRs)
  ).

trs_for_data({_I, []}, _TDsFun, TRs) ->
  TRs;
trs_for_data({I, Elems}, TDsFun, TRs) ->
  [{tr, [], [{td, [], [integer_to_list(I)]}] ++ TDsFun(Elems)} | TRs].

tds_for_data(Elems) ->
  [{td, [], join_br(Elems)}].

trs_for_kbukets({_I, {[], []}}, _TDsFun, TRs) ->
  TRs;
trs_for_kbukets({I, Elems}, TDsFun, TRs) ->
  [{tr, [], [{td, [], [integer_to_list(I)]}] ++ TDsFun(Elems)} | TRs].

tds_for_kbukets({Nodes, AddNodes}) ->
  lists:map(
    fun (Nodes) -> {td, [], join_br(Nodes)} end,
    [Nodes, AddNodes]
  ).

join_br(Lists) ->
  list_utils:join(
    {br}, lists:map(fun (Elem) -> io_lib:write(Elem) end, Lists)
  ).

docroot() ->
  {file, Path} = code:is_loaded(?MODULE),
  filename:join([filename:dirname(filename:dirname(Path)), "priv", "www"]).

