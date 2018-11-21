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

-export([call/2, call/3, make_call/3, make_call/4, cast/2, make_cast/3]).
%-export([ parallel_r/4,parallel_nr/3]).
-define(SERVER, ?MODULE).
-define(CONNECTION_TIMEOUT, 5000).
-define(RECEIVE_TIMEOUT,15000).
-record(state, {}).

start_link(Services) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Services], []).

init([Services]) ->
    ets:new(?MODULE, [set, named_table, protected,{write_concurrency,false}, {read_concurrency,true}]),  
    start_services(Services), 
    {ok, #state{}}.

start_services(Services) ->
    InitFun =  fun({Alias, Url, Port, PoolTimeout, PoolConnections}) ->
		       ets:insert(?MODULE,{Alias, iolist_to_binary([Url, <<":">>, Port])} ),
		       PoolName = Alias,
		       Options = [{timeout,  PoolTimeout}, {max_connections, PoolConnections}],
		       R = hackney_pool:start_pool(PoolName, Options),

		       io:format("~p Started pool ~p: ~p~n",[R,PoolName,Options])
	       end,
    lists:foreach(InitFun, Services).


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% CAST
cast({Register, ServerAlias},{Path}) ->
    cast({Register, ServerAlias},{Path, []});

cast({_Register, ServerAlias},{Path, ArgList}) ->
    URL = iolist_to_binary([get_address(ServerAlias), <<"/cast/">>, atom_to_binary(ServerAlias,utf8), <<"/">>, Path]),
    %%p_cast(node(),?MODULE,make_cast,[ServerAlias, URL, ArgList]),
    make_cast(ServerAlias, URL, ArgList).

make_cast(ServerAlias, URL, ArgList) ->
    Headers = [],
    Payload = encode_arglist(ArgList, []),
    Options = [{pool, ServerAlias},{connect_timeout, ?CONNECTION_TIMEOUT}, {recv_timeout, ?RECEIVE_TIMEOUT}],
    post(cast,URL, Headers,Payload, Options).

%% CALL
call({_Register, ServerAlias},{Path, ArgList},Timeout) ->
    URL = iolist_to_binary([get_address(ServerAlias), <<"/call/">>, atom_to_binary(ServerAlias, utf8), <<"/">>, Path]),
    %%p_call(node(),From,?MODULE,make_call,[ServerAlias, URL, ArgList]),
    make_call(ServerAlias, URL, ArgList, Timeout).
				   
call(Where,{Path}) ->
    call(Where,{Path, []},?RECEIVE_TIMEOUT);

call(Where, {Path, ArgList}) ->
   call(Where, {Path, ArgList},?RECEIVE_TIMEOUT).

make_call(ServerAlias, URL, ArgList) ->
	make_call(ServerAlias, URL, ArgList, ?RECEIVE_TIMEOUT).
    
make_call(ServerAlias, URL, ArgList, TimeOut) ->
    Headers = [],
    Payload = encode_arglist(ArgList, []),
    Options = [{pool, ServerAlias},{connect_timeout, ?CONNECTION_TIMEOUT}, {recv_timeout, TimeOut}], 
    post(call,URL, Headers,Payload, Options).

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

post(call, URL, Headers,Payload, Options) ->
    case hackney:request(post,URL, Headers, Payload, Options) of
	{ok, _StatusCode, _RespHeaders, ClientRef} ->
	    case hackney:body(ClientRef) of
		{ok, Body} ->
		    binary_to_term(Body);
		ErrorBody ->
		    io:format("~p ~p~n",[URL,ErrorBody]),
		    <<>>
	    end;
	ErrorPost ->
	    io:format("~p ~p~n",[URL,ErrorPost]),
	    <<>>
    end;

post(cast,URL, Headers,Payload, Options) ->	
    case hackney:request(post,URL, Headers, Payload, Options) of
        {ok, _StatusCode, _RespHeaders, ClientRef} ->
	    case hackney:body(ClientRef) of
                {ok, _} ->
                    <<>>;
                ErrorBody ->
                    io:format("~p ~p~n",[URL,ErrorBody]),
                    <<>>
	     end;
	ErrorPost ->
	    io:format("~p ~p~n",[URL,ErrorPost]),
            <<>>
    end.



%% AUX
encode_arglist([], R) ->
    term_to_binary(lists:reverse(R));
encode_arglist([H|T], R) ->
    encode_arglist( T, [term_to_binary(H)|R]).




%%PARALLEL CALL
%%p_call(Node, From, M, F, A) ->
  %% spawn(Node,?MODULE, parallel_r,[From, M,F,A]).

%%p_cast(Node, M, F, A) ->
  %%  spawn(Node,?MODULE, parallel_nr,[ M,F,A]).

%%parallel_r(From, M, F, A) ->
  %%  Reply = erlang:apply(M,F,A),
  %%  gen_server:reply(From, Reply).

%%parallel_nr( M, F, A) ->
    %%erlang:apply(M,F,A).

