-module(balance_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-record(account, {user_id, balance}).

init(Req0, State) ->
    %% Only accept GET
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            handle_get(Req0, State);
        _ ->
            Req = reply_json(Req0, 405, #{error => <<"Method not allowed">>}),
            {ok, Req, State}
    end.

handle_get(Req0, State) ->
    try
        {ok, UserId, _IsAdmin} = jwt_helper:validate_jwt(Req0),
        Balance = get_balance(UserId),
        Resp = reply_json(Req0, 200, #{
            user_id => UserId,
            balance => Balance
        }),
        {ok, Resp, State}
    catch
        error:_ ->
            {ok, reply_json(Req0, 500, #{error => <<"internal_error">>}), State}
    end.

get_balance(UserId) ->
    F = fun() ->
        case mnesia:read(account, UserId) of
            [#account{balance = Balance}] -> 
                Balance;
            [] -> 
                %% Account doesn't exist, create with initial balance
                InitialBalance = application:get_env(betting_node, initial_balance, 100.0),
                NewAccount = #account{user_id = UserId, balance = InitialBalance},
                mnesia:write(NewAccount),
                InitialBalance
        end
    end,
    {atomic, Balance} = mnesia:transaction(F),
    Balance.

reply_json(Req, Status, Body) ->
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body),
        Req).
