%%% @author Moymer
%%% @copyright (C) 2017, Moymer
%%% @doc
%%%
%%% @end
%%% Created :  1 Mar 2017 by Moymer 

-module(http_gen_server).
-export([init/0,add_gen_server/3, call/2, cast/2, call_rec/0, aws_stats/0, elastic_stats/0]).
-include_lib("awesome.hrl").

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
    Headers = [],
    Payload = ArgList,
    Options = [{pool, ServerAlias}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:post(URL, Headers,
								Payload, Options),
    {ok, Body} = hackney:body(ClientRef),
    Body.

call({global, ServerAlias},{Path, ArgList}) ->
    URL = iolist_to_binary([get_address(ServerAlias), <<"/">>,Path]),
    make_call(ServerAlias, URL, ArgList);

call({local, ServerAlias},{Path, ArgList}) ->
    URL = iolist_to_binary([ <<"127.0.0.1:8080">>, <<"/call/">>,Path]),
    make_call(ServerAlias, URL, ArgList).

make_call(ServerAlias, URL, ArgList) ->
    Headers = [],
    Payload =  ArgList,
    Options = [{pool, ServerAlias}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:post(URL, Headers,
							       Payload, Options),
    {ok, Body} = hackney:body(ClientRef),
    Body.

aws_stats() ->
    http_gen_server:cast({local, local}, {"awscloud", <<>>}).

elastic_stats() ->
    http_gen_server:cast({local, local}, {"elastic", <<>>}).

call_rec() ->
    A = #user{
              userId = <<"123">>,
	          twitterId = <<"1234">>,
	          facebookId = <<"12345">>,
	          signupIp = <<"123.123.212">>,
              username = <<"Jofrey">>,
              bio  = <<"@@@ #$% insafinia @inad">>,
	          photoUrl = <<"wwww.com.br">>,
              signupDate = 12301,
	          mainLocale = <<"Sp">>,
              likeCount = 12,
              featuredCount = 12,
	          followerCount = 12,
	          followeeCount = 12,
	          viewsCount = 12,
	          isVerified = 12,
	          isAdm = 0,
	          status = 1 
             },
    B = [term_to_binary(A)],
    http_gen_server:call({local, local}, {"awscloud_monitor/get_info", term_to_binary(B)}).

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
