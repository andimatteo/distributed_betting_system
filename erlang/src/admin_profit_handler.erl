-module(admin_profit_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-record(account, {user_id, balance}).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            handle_get(Req0, State);
        _ ->
            Req = reply_json(Req0, 405, #{error => <<"Method not allowed">>}),
            {ok, Req, State}
    end.

handle_get(Req0, State) ->
    case jwt_helper:validate_jwt(Req0) of
        {ok, _UserId, true} ->  %% Must be admin
            try
                Profit = get_bookmaker_profit(),
                Resp = reply_json(Req0, 200, #{
                    profit => Profit
                }),
                {ok, Resp, State}
            catch
                error:_ ->
                    {ok, reply_json(Req0, 500, #{error => <<"internal_error">>}), State}
            end;
        {ok, _UserId, false} ->
            Req = reply_json(Req0, 403, #{error => <<"Admin access required">>}),
            {ok, Req, State};
        {error, missing_token} ->
            Req = reply_json(Req0, 401, #{error => <<"Missing authorization token">>}),
            {ok, Req, State};
        {error, _Reason} ->
            Req = reply_json(Req0, 401, #{error => <<"Invalid or expired token">>}),
            {ok, Req, State}
    end.

get_bookmaker_profit() ->
    BookmakerId = <<"bookmaker">>,
    F = fun() ->
        case mnesia:read(account, BookmakerId) of
            [#account{balance = Balance}] -> 
                Balance;
            [] -> 
                %% Initialize if missing
                BookmakerMoney = application:get_env(betting_node, bookmaker_money, 0),
                NewAccount = #account{user_id = BookmakerId, balance = BookmakerMoney},
                mnesia:write(NewAccount),
                BookmakerMoney
        end
    end,
    {atomic, Profit} = mnesia:transaction(F),
    Profit.

reply_json(Req, Status, Body) ->
    NodeName = list_to_binary(atom_to_list(node())),
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>,
          <<"x-erlang-node">> => NodeName},
        jsx:encode(Body),
        Req).
