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
        error:account_not_found ->
            {ok, reply_json(Req0, 404, #{error => <<"Account not found">>}), State};
        error:_ ->
            {ok, reply_json(Req0, 500, #{error => <<"internal_error">>}), State}
    end.

get_balance(UserId) ->
    F = fun() ->
        mnesia:read(account, UserId)
    end,
    {atomic, Rows} = mnesia:transaction(F),
    case Rows of
        [#account{balance = Balance}] -> Balance;
        [] -> erlang:error(account_not_found)
    end.

reply_json(Req, Status, Body) ->
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body),
        Req).
