-module(websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([websocket_terminate/3]).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{idle_timeout => 60000}}.

websocket_init(State) ->
    %% Register as anonymous WebSocket connection
    global:register_name({ws, node(), self()}, self()),
    io:format("WebSocket connection registered: ~p~n", [self()]),
    {[], State#{user_id => undefined}}.

websocket_handle({text, Msg}, State) ->
    try jsx:decode(Msg, [return_maps]) of
        Json ->
            case maps:get(<<"opcode">>, Json, undefined) of
                <<"keepalive">> ->
                    Reply = jsx:encode(#{<<"opcode">> => <<"keepalive">>}),
                    {[{text, Reply}], State};
                <<"authenticate">> ->
                    %% Client sends JWT to authenticate and receive personal updates
                    Token = maps:get(<<"token">>, Json, <<>>),
                    case authenticate_user(Token) of
                        {ok, UserId} ->
                            %% Unregister anonymous, register with user ID
                            global:unregister_name({ws, node(), self()}),
                            global:register_name({ws_user, node(), UserId, self()}, self()),
                            Reply = jsx:encode(#{
                                <<"opcode">> => <<"authenticated">>,
                                <<"user_id">> => UserId
                            }),
                            {[{text, Reply}], State#{user_id => UserId}};
                        {error, Reason} ->
                            Reply = jsx:encode(#{
                                <<"opcode">> => <<"error">>,
                                <<"message">> => atom_to_binary(Reason, utf8)
                            }),
                            {[{text, Reply}], State}
                    end;
                _ ->
                    {[], State}
            end
    catch
        _:_ ->
            Reply = jsx:encode(#{
                <<"opcode">> => <<"error">>, 
                <<"message">> => <<"invalid json">>
            }),
            {[{text, Reply}], State}
    end;

websocket_handle(_Data, State) ->
    {[], State}.

websocket_info({broadcast, Msg}, State) ->
    {[{text, Msg}], State};

websocket_info({timeout, _Ref, Msg}, State) ->
    {[{text, Msg}], State};

websocket_info(_Info, State) ->
    {[], State}.

websocket_terminate(_Reason, _Req, State) ->
    io:format("WebSocket connection closed: ~p~n", [self()]),
    case maps:get(user_id, State, undefined) of
        undefined ->
            global:unregister_name({ws, node(), self()});
        UserId ->
            global:unregister_name({ws_user, node(), UserId, self()})
    end,
    ok.

%% Authenticate user from JWT token
authenticate_user(Token) ->
    Secret = get_jwt_secret(),
    case jwt_validator:validate(Token, Secret) of
        {ok, Claims} ->
            UserId = maps:get(<<"id">>, Claims, undefined),
            case UserId of
                undefined -> {error, invalid_claims};
                _ -> {ok, UserId}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_jwt_secret() ->
    case os:getenv("JWT_SECRET") of
        false ->
            case file:consult("secrets.config") of
                {ok, [{jwt_secret, Secret}]} -> Secret;
                _ -> application:get_env(betting_node, jwt_secret, <<"your_secret_key_here">>)
            end;
        EnvSecret ->
            list_to_binary(EnvSecret)
    end.
