-module(gen_server_connector).

-export([start_connector/0,init/2]).
-export([content_types_provided/2]).
-export([hello_to_html/2]).
-export([hello_to_json/2]).
-export([hello_to_text/2]).


start_connector() ->
    Dispatch = cowboy_router:compile([
				      {'_', [
					     {"/", gen_server_connector, []}
					    ]}
				     ]),
    {ok, _} = cowboy:start_clear(http, 100, [{port, 8080}], #{
					      env => #{dispatch => Dispatch}
					     }).



init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, hello_to_html},
      {<<"application/json">>, hello_to_json},
      {<<"text/plain">>, hello_to_text}
      ], Req, State}.

hello_to_html(Req, State) ->

    Body = <<"<html>
              <head>
               <meta charset=\"utf-8\">
               <title>REST Hello World!</title>
              </head>
              <body>
                <p>REST Hello World as HTML!</p>
              </body>
           </html>">>,
    {Body, Req, State}.

hello_to_json(Req, State) ->
    Body = <<"{\"rest\": \"Hello World!\"}">>,
    {Body, Req, State}.

hello_to_text(Req, State) ->
    {<<"REST Hello World as text!">>, Req, State}.
    
