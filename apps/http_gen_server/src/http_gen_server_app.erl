%%%-------------------------------------------------------------------
%% @doc http_gen_server public API
%% @end
%%%-------------------------------------------------------------------

-module(http_gen_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    application:ensure_all_started(cowboy),
    application:ensure_all_started(hackney),
    gen_server_connector:start_connector(),
    http_gen_server_sup:start_link().


%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
