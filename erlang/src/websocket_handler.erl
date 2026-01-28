-module(websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([websocket_terminate/3]).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{idle_timeout => 60000}}.

websocket_init(_State) ->
    %% Register as anonymous WebSocket connection
    global:register_name({ws, node(), self()}, self()),
    io:format("WebSocket connection registered: ~p~n", [self()]),
    {[], #{user_id => undefined, is_admin => false}}.

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
                        {ok, UserId, IsAdmin} ->
                            %% Unregister anonymous, register with user ID
                            global:unregister_name({ws, node(), self()}),
                            RegisterName = case IsAdmin of
                                true -> {ws_admin, node(), UserId, self()};
                                false -> {ws_user, node(), UserId, self()}
                            end,
                            global:register_name(RegisterName, self()),
                            Reply = jsx:encode(#{
                                <<"opcode">> => <<"authenticated">>,
                                <<"user_id">> => UserId,
                                <<"is_admin">> => IsAdmin
                            }),
                            {[{text, Reply}], State#{user_id => UserId, is_admin => IsAdmin}};
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
            IsAdmin = maps:get(is_admin, State, false),
            case IsAdmin of
                true -> global:unregister_name({ws_admin, node(), UserId, self()});
                false -> global:unregister_name({ws_user, node(), UserId, self()})
            end
    end,
    ok.

%% Authenticate user from JWT token
authenticate_user(Token) ->
    Secret = get_jwt_secret(),
    case jwt_validator:validate(Token, Secret) of
        {ok, Claims} ->
            UserId = maps:get(<<"id">>, Claims, undefined),
            IsAdmin = maps:get(<<"isAdmin">>, Claims, false),
            case UserId of
                undefined -> {error, invalid_claims};
                _ -> {ok, UserId, IsAdmin}
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
