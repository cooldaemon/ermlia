%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc UDP Server Include file.

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

%% @type udp_server_option() =
%%  {option(), port(), max_restarts(), time(), shutdown(),
%%   recv_length(), recv_timeout()}.
%%  A data structure holding the options.
%% @type option()       = [term()].
%% @type port()         = integer().
%% @type max_restarts() = integer().
%% @type time()         = integer().
%% @type shutdown()     = integer().
%% @type recv_length()  = integer().
%% @type recv_timeout() = integer() | infinity.
-record(udp_server_option, {
  option = [binary],
  port = 4000,
  max_restarts = 3,
  time = 60,
  shutdown = 2000,
  recv_length = 0,
  recv_timeout = infinity
}).
 
