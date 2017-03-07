%%% @author Moymer
%%% @copyright (C) 2017, Moymer
%%% @doc
%%%
%%% @end
%%% Created :  1 Mar 2017 by Moymer 

-module(http_gen_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([add_gen_server/3, call/2, cast/2]).

-define(SERVER, ?MODULE).
-record(state, {}).

start_link(Services) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Services], []).

init([Services]) ->
    ets:new(?MODULE, [set, named_table, protected]),  
    start_services(Services), 
    {ok, #state{}}.

start_services(Services) ->
    InitFun =  fun({Alias, Url, Port, PoolTimeout, PoolConnections}) ->
		       ets:insert(?MODULE,{Alias, iolist_to_binary([Url, <<":">>, Port])} ),
		       PoolName = Alias,
		       Options = [{timeout,  PoolTimeout}, {max_connections, PoolConnections}],
		       hackney_pool:start_pool(PoolName, Options)
	       end,
    lists:foreach(InitFun, Services).

handle_call({call, local, ServerAlias, Path, ArgList}, _From, State) ->
    URL = iolist_to_binary([ <<"127.0.0.1:8080">>, <<"/call/">>, atom_to_binary(ServerAlias, utf8), <<"/">>, Path]),
    Reply = make_call(ServerAlias, URL, ArgList),
    {reply, Reply, State};

handle_call({call, global, ServerAlias, Path, ArgList}, _From, State) ->
    URL = iolist_to_binary([get_address(ServerAlias), <<"/call/">>, atom_to_binary(ServerAlias, utf8), <<"/">>, Path]),
    Reply = make_call(ServerAlias, URL, ArgList),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({cast, global, ServerAlias, Path, ArgList}, State) ->
    URL = iolist_to_binary([get_address(ServerAlias), <<"/cast/">>, atom_to_binary(ServerAlias,utf8), <<"/">>, Path]),
    make_cast(ServerAlias, URL, ArgList),
    {noreply,State};

handle_cast({cast, local, ServerAlias, Path, ArgList}, State) ->
    URL = iolist_to_binary([ <<"127.0.0.1:8080">>, <<"/cast/">>, atom_to_binary(ServerAlias,utf8), <<"/">>, Path]),
    make_cast(ServerAlias, URL, ArgList),
    {noreply,State};

handle_cast({add_gen_server, Alias, Url, Port},  State) ->
    ets:insert(?MODULE,{Alias, iolist_to_binary([Url, <<":">>, Port])} ),
    PoolName = Alias,   
    Options = [{timeout,  300000}, {max_connections, 1000}],
    hackney_pool:start_pool(PoolName, Options),
   {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


add_gen_server(Alias, Url, Port) ->
   gen_server:cast(?MODULE,{add_gen_server,Alias, Url, Port}).
    

cast({Register, ServerAlias},{Path, ArgList}) ->
    gen_server:cast(?MODULE,{cast, Register, ServerAlias, Path, ArgList}).

make_cast(ServerAlias, URL, ArgList) ->
    Headers = [],
    Payload = encode_arglist(ArgList, []),
    Options = [{pool, ServerAlias}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:post(URL, Headers,
								Payload, Options),
    {ok, Body} = hackney:body(ClientRef),
    Body.

call({Register, ServerAlias},{Path, ArgList}) ->
    gen_server:call(?MODULE,{call, Register, ServerAlias, Path, ArgList}).

make_call(ServerAlias, URL, ArgList) ->
    Headers = [],
    Payload = encode_arglist(ArgList, []),
    Options = [{pool, ServerAlias}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:post(URL, Headers,
							       Payload, Options),
    {ok, Body} = hackney:body(ClientRef),
    binary_to_term(Body).

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

%% AUX
encode_arglist([], R) ->
    term_to_binary(lists:reverse(R));
encode_arglist([H|T], R) ->
    encode_arglist( T, [term_to_binary(H)|R]).
