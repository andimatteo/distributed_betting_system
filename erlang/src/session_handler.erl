-module(session_handler).

-behaviour(cowboy_handler).

-export([init/2]).


-record(account, {user_id, balance}).

init(Req0, State) ->
    %% Only accept POST
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            handle_post(Req0, State);
        _ ->
            Req = reply_json(Req0, 405, #{error => <<"Method not allowed">>}),
            {ok, Req, State}
    end.

handle_post(Req0, State) ->
    try
        {ok, UserId, _IsAdmin} = jwt_helper:validate_jwt(Req0),
        InitialBalance = application:get_env(betting_node, initial_balance, 100.0),

        Resp = case account_exists(UserId) of
            true ->
                reply_json(Req0, 200, #{
                    message => <<"Account already exists">>,
                    user_id => UserId
                });
            false ->
                ok = create_account(UserId, InitialBalance),
                reply_json(Req0, 201, #{
                    message => <<"Account created">>,
                    user_id => UserId,
                    balance => InitialBalance
                })
        end,
        {ok, Resp, State}
    catch
        error:_ ->
            {ok, reply_json(Req0, 500, #{error => <<"internal_error">>}), State}
    end.

account_exists(UserId) ->
    F = fun() ->
        mnesia:read(account, UserId)
    end,
    {atomic, Rows} = mnesia:transaction(F),
    case Rows of
        [] -> false;
        [_Account] -> true
    end.

create_account(UserId, InitialBalance) ->
    Account = #account{
        user_id = UserId,
        balance = InitialBalance
    },
    F = fun() ->
        mnesia:write(Account)
    end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

reply_json(Req, Status, Body) ->
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body),
        Req).
