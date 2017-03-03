-module(gen_server_connector).

-export([start_connector/0, init/2]).
-export([allowed_methods/2]).
-include_lib("awesome.hrl").

start_connector() ->
    Dispatch = cowboy_router:compile([
				      {'_', [
					     {"/", gen_server_connector, []}, 
                         {"/[...]", gen_server_connector, []}
					    ]}
				     ]),
    {ok, _} = cowboy:start_clear(http, 100, [{port, 8080}], #{
					      env => #{dispatch => Dispatch}
					     }).

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>], Req, State}.

init(Req, Opts) ->
    Path = cowboy_req:path(Req),
	Request = handle_request(Path, Req),
	{ok, Request, Opts}.
	

handle_request(<<"/Record">>, Req) ->
    {ok, Value, Req0} = cowboy_req:read_body(Req, #{}),
    %%C = binary_to_term(Value),
    [A, Record, B | T] = binary_to_term(Value),
    %%io:format("Dec ~p~n", [C]),
    io:format("Int ~p~n", [binary_to_term(A)]),
    io:format("Bin ~p~n", [binary_to_term(B)]),
    Record2 = binary_to_term(Record),
    io:format("Record ~p~n", [Record2#user.signupDate]);

handle_request(<<"/Binary">>, Req) ->
    {ok, Value, Req0} = cowboy_req:read_body(Req, #{}),
    io:format("Bin ~p~n", [Value]);

handle_request(<<"/Integer">>, Req) ->
    {ok, [{Value, _}], Req0} = cowboy_req:read_body(Req),
io:format("Integer ~p~n", [binary_to_integer(Value)]).
%%list_to_integer(binary_to_list(Value))

hello_to_json(<<"POST">>, Path, Req) ->
    Body = <<"{\"rest\": \"Hello World!\"}">>,
    cowboy_req:reply(200, #{ <<"content-type">> => <<"text/plain">> }, Path, Req).
