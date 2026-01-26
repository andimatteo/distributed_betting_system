-module(jwt_helper).

-export([validate_jwt/1, validate_admin_jwt/1, get_user_id/1]).

%% Get JWT secret from secrets.config file or environment variable
get_jwt_secret() ->
    %% Priority: 1. Environment variable, 2. secrets.config file, 3. app config
    case os:getenv("JWT_SECRET") of
        false ->
            case file:consult("secrets.config") of
                {ok, [{jwt_secret, Secret}]} -> Secret;
                _ -> application:get_env(betting_node, jwt_secret, <<"your_secret_key_here">>)
            end;
        EnvSecret ->
            list_to_binary(EnvSecret)
    end.

%% Validate JWT and return user_id and isAdmin flag
validate_jwt(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            {error, missing_token};
        AuthHeader ->
            Secret = get_jwt_secret(),
            case jwt_validator:validate_bearer(AuthHeader, Secret) of
                {ok, Claims} ->
                    UserId = maps:get(<<"id">>, Claims, undefined),
                    IsAdmin = maps:get(<<"isAdmin">>, Claims, false),
                    case UserId of
                        undefined -> {error, invalid_claims};
                        _ -> {ok, UserId, IsAdmin}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% Validate JWT and ensure user is admin
validate_admin_jwt(Req) ->
    case validate_jwt(Req) of
        {ok, UserId, true} -> {ok, UserId};
        {ok, _UserId, false} -> {error, not_admin};
        {error, Reason} -> {error, Reason}
    end.

%% Get user_id from JWT
get_user_id(Req) ->
    case validate_jwt(Req) of
        {ok, UserId, _IsAdmin} -> {ok, UserId};
        {error, Reason} -> {error, Reason}
    end.
