-module(gen_server_connector).

-export([start_connector/0, init/2, allowed_methods/2]).

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
	Request = handle_request(Req),
    {ok, Request, Opts}.

handle_request(Req) ->
    Path = cowboy_req:path(Req),
    [GenFunc, ModToken, ModFuncToken] = string:tokens(binary_to_list(Path), "/"),
    Module = list_to_atom(ModToken),
    ModuleFunc = list_to_atom(ModFuncToken),
    {ok, Body, Request} = cowboy_req:read_body(Req, #{}),
    [Args | _] = binary_to_term(Body),
    case GenFunc of 
        "call" ->
            Response = gen_server:call(Module, {ModuleFunc, Args}),
            cowboy_req:reply(200, #{}, term_to_binary(Response), Request);
        "cast" -> 
            Response = gen_server:call(Module, {ModuleFunc, Args}),
            cowboy_req:reply(200, #{}, term_to_binary(Response), Request);
        _ -> 
            cowboy_req:reply(200, #{}, "No method found", Request)
    end.
