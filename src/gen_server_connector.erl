-module(gen_server_connector).

-export([start_connector/1, start_connector/2, init/2]).

-define(TIMEOUT,30000).

start_connector(Port, Connections) -> 
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/", gen_server_connector, []},
					     {"/[...]", gen_server_connector, []}
                                            ]}
				     ]),
    {ok, _} = cowboy:start_clear(http, [{port, Port}, {num_acceptors,Connections}], #{
						      env => #{dispatch => Dispatch}, request_timeout => 120000, max_keepalive => 1000
						     }).

start_connector(Port) ->
    start_connector(Port, 1000).


init(Req, Opts) ->
    Request = handle_request(Req),
    {ok, Request, Opts}.

handle_request(Req) ->
    Path = cowboy_req:path(Req),
    [_,GenFunc, ModToken, ModFuncToken] = binary:split(Path,<<"/">>,[global]),  
    Module = binary_to_atom(ModToken,utf8),
    ModuleFunc = binary_to_atom(ModFuncToken,utf8),
    {ok, Body, Request} = cowboy_req:read_body(Req, #{}),
    ArgList = [ModuleFunc | decode_arglist(binary_to_term(Body), [])],
    Args = list_to_tuple(ArgList),
    %%io:format("PID ~p, ~p ~p for ~p ~n",[self(),GenFunc,Module, Args ]),
    case GenFunc of 
        <<"call">> ->
	    try
		R = gen_server:call({global,Module}, Args, ?TIMEOUT ),
		{ok, R}    
	    of
		{ok, Response} -> 
		    cowboy_req:reply(200, #{}, term_to_binary(Response), Request),
		    Request	
	    catch
	     _Error:Reason -> 
		    io:format("~p Error: ~p ~p~n",[ModuleFunc,Reason,erlang:get_stacktrace()]),
		    empty_answer(Request)
			
	    end;
	    %%io:format("Call response ~p~n",[Response]),
	   
        <<"cast">> -> 
	    try
		gen_server:cast({global,Module}, Args),
                {ok, done}
            of
                {ok, done} -> 
		    empty_answer(Request)
	    catch
              _Error:Reason -> 
		    io:format("~p Error: ~p ~p~n",[ModuleFunc,Reason,erlang:get_stacktrace()]),
                    empty_answer(Request)
	    end;
	_ -> 
            cowboy_req:reply(404, #{}, "No method found", Request)
    end.

empty_answer(Request) ->
    cowboy_req:reply(200, #{}, term_to_binary(<<>>), Request),
    Request.

decode_arglist([], R) ->
    lists:reverse(R);
decode_arglist([H|T], R) ->
    decode_arglist( T, [binary_to_term(H)|R]).
