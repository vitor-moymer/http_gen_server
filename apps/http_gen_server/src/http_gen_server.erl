%%% @author Moymer
%%% @copyright (C) 2017, Moymer
%%% @doc
%%%
%%% @end
%%% Created :  1 Mar 2017 by Moymer 

-module(http_gen_server).
-export([init/0,add_gen_server/3,call/2, cast/2]).




%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

init() ->
    ets:new(?MODULE, [set, named_table, protected]),
    add_gen_server(local, <<"127.0.0.1">>, <<"8080">>).


add_gen_server(Alias, Url, Port) ->
    ets:insert(?MODULE,{Alias, iolist_to_binary([Url, <<":">>, Port])} ),
    PoolName = Alias,
    Options = [{timeout, 300000}, {max_connections, 1000}],
    hackney_pool:start_pool(PoolName, Options).
    


cast({global, ServerAlias},{Path, ArgList}) ->
    URL = iolist_to_binary([get_address(ServerAlias), <<"/">>,Path]),
    make_call(ServerAlias, URL, ArgList);

cast({local, ServerAlias},{Path, ArgList}) ->
    URL = iolist_to_binary([ <<"127.0.0.1:8080">>, <<"/">>,Path]),
    make_cast(ServerAlias, URL, ArgList).

make_cast(ServerAlias, URL, ArgList) ->
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Payload =  mochijson2:encode(ArgList),
    Options = [{pool, ServerAlias}],
    {ok, _StatusCode, _RespHeaders, _ClientRef} = hackney:post( URL, Headers,
								Payload, Options).




call({global, ServerAlias},{Path, ArgList}) ->
    URL = iolist_to_binary([get_address(ServerAlias), <<"/">>,Path]),
    make_call(ServerAlias, URL, ArgList);

call({local, ServerAlias},{Path, ArgList}) ->
    URL = iolist_to_binary([ <<"127.0.0.1:8080">>, <<"/">>,Path]),
    make_call(ServerAlias, URL, ArgList).

make_call(ServerAlias, URL, ArgList) ->
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Payload =  mochijson2:encode(ArgList),
    Options = [{pool, ServerAlias}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:post( URL, Headers,
							       Payload, Options),
    {ok, Body} = hackney:body(ClientRef),
    Body.


get_address(ServerAlias) ->
    Default = <<"127.0.0.1:8080">>, 
    case ets:lookup(?MODULE, ServerAlias) of
        [] ->
	    Default;
	[{_, undefined}] ->
            Default;
        [{_, V}] ->
            V
    end.
