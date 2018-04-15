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
-module(etcp_metadata).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([wrap/3
        ,unwrap/1
        ,transporter/1
        ,socket/1
        ,transporter_options/1]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-include("internal/etcp_metadata.hrl").
-define(ETCP_MD, etcp_metadata).

%% -------------------------------------------------------------------------------------------------
%% API:


wrap(TrMod, Sock, TrOpts) ->
    #?ETCP_MD{transporter = TrMod
             ,socket = Sock
             ,transporter_options = TrOpts}.



unwrap(#?ETCP_MD{transporter = TrMod
                ,socket = Sock
                ,transporter_options = TrOpts}) ->
    {TrMod, Sock, TrOpts}.


-spec
transporter(etcp_types:metadata()) ->
    module().
transporter(#?ETCP_MD{transporter = TrMod}) ->
    TrMod.


-spec
transporter_options(etcp_types:metadata()) ->
    etcp_types:start_options().
transporter_options(#?ETCP_MD{transporter_options = TrOpts}) ->
    TrOpts.


-spec
socket(etcp_types:metadata()) ->
    etcp_types:start_options().
socket(#?ETCP_MD{socket = Sock}) ->
    Sock.