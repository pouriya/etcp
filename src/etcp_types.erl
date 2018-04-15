%%% ------------------------------------------------------------------------------------------------
%%% ETCP is available for use under the following license, commonly known as the 3-clause (or
%%% "modified") BSD license:
%%%
%%% Copyright (c) 2017-2018, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted
%%% provided that the following conditions are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice, this list of
%%%    conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright notice, this list of
%%%    conditions and the following disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its contributors may be used to
%%%    endorse or promote products derived from this software without specific prior written
%%%    permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%%% FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ------------------------------------------------------------------------------------------------
%% @author  Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version 17.9.10
%% -------------------------------------------------------------------------------------------------
-module(etcp_types).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-include("internal/etcp_metadata.hrl").

%% -------------------------------------------------------------------------------------------------
%% Types:

-type name() :: pid() | atom() | tuple().

-type start_return() :: {'ok', pid()} | {'error', term()} | 'ignore'.

-type register_name() :: {'local', atom()}
                       | {'global', atom()}
                       | {'via', module(), term()}.

-type error() :: {'error', {atom(), list()}} | {'error', term()}.

-type transporter_callback_error() :: {'error', [] | [{atom(), any()}]}.

-type socket() :: any(). % Depends on transporter module

-type start_options() :: #{'acceptor_count' => pos_integer()
                          ,'acceptor_mode' => acceptor_mode()
                          ,'acceptor_debug' => debug()
                          ,'transporter' => module()
                          ,'transporter_options' => transporter_options()
                          ,'connection_debug' => debug()
                          ,'connection_process_registry' => boolean()}.

-type acceptor_mode() :: 'accept' | 'sleep'.

-type debug() :: [sys:dbg_opt()] | [].

-type transporter_options() :: term(). % Depends on transporter module
                                       % for etcp_transporter_tcp module use gen_tcp:options()
                                       % for etcp_transporter_ssl module use ssl:options()

-type port_number() :: inet:port_number().

-type host() :: inet:hostname() | inet:socket_address().

-type addresses() :: [] | [address()].
-type  address() :: {host(), port_number()} | {'undefined', 'undefined'}.

-type packet() :: any(). % Depends on transporter module
                         % for etcp_transporter_tcp etcp_transporter_ssl modules:
                         %  string() | binary() | iolist()

-type shutdown_type() :: 'read' | 'write' | 'read_write'.

-type error_info() :: string().

-type length() :: integer().

-type metadata() :: #etcp_metadata{}.

-type init_argument() :: any().
-type state() :: any().
-type callback_return() :: 'ok'
                         | {'ok', callback_return_options()}
                         | 'close'
                         | {'close', callback_return_options()}
                         | {'stop', Reason::reason()}
                         | {'stop', Reason::reason(), callback_return_options()}.
-type callback_return_options() :: [] | [callback_return_option()].
-type  callback_return_option() :: {'state', any()}
                                 | {'timeout', timeout()}
                                 | {'packet', etcp_types:packet()}
                                 | {'transporter', module()}
                                 | {'socket', etcp_types:socket()}
                                 | {'setopts', any()}
                                 | {'hibernate', boolean()}.

-type reason() :: any().
-type terminate_callback_return() :: {'new_error', NewReason::reason()} | any().
-type code_change_callback_return() :: {'ok', state()}.
-type listen_init_callback_return() :: 'ok'
                                     | {'ok', init_argument()}
                                     | {'stop', reason()}
                                     | 'ignore'.

-export_type([name/0
             ,start_return/0
             ,register_name/0
             ,error/0
             ,socket/0
             ,start_options/0
             ,port_number/0
             ,host/0
             ,packet/0
             ,shutdown_type/0
             ,error_info/0
             ,length/0
             ,metadata/0
             ,acceptor_mode/0
             ,addresses/0
             ,address/0
             ,callback_return_options/0
             ,callback_return_option/0
             ,init_argument/0
             ,state/0
             ,callback_return/0
             ,terminate_callback_return/0
             ,code_change_callback_return/0
             ,listen_init_callback_return/0
             ,transporter_callback_error/0
             ,debug/0
             ,transporter_options/0]).