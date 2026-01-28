-module(balance_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-record(account, {user_id, balance}).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            handle_get(Req0, State);
        <<"POST">> ->
            handle_post(Req0, State);
        _ ->
            Req = reply_json(Req0, 405, #{error => <<"Method not allowed">>}),
            {ok, Req, State}
    end.

handle_get(Req0, State) ->
    case jwt_helper:validate_jwt(Req0) of
        {ok, UserId, _IsAdmin} ->
            try
                Balance = get_balance(UserId),
                Resp = reply_json(Req0, 200, #{
                    user_id => UserId,
                    balance => Balance
                }),
                {ok, Resp, State}
            catch
                error:_ ->
                    {ok, reply_json(Req0, 500, #{error => <<"internal_error">>}), State}
            end;
        {error, missing_token} ->
            Req = reply_json(Req0, 401, #{error => <<"Missing authorization token">>}),
            {ok, Req, State};
        {error, _Reason} ->
            Req = reply_json(Req0, 401, #{error => <<"Invalid or expired token">>}),
            {ok, Req, State}
    end.

handle_post(Req0, State) ->
    case jwt_helper:validate_jwt(Req0) of
        {ok, UserId, _IsAdmin} ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            try
                Json = jsx:decode(Body, [return_maps]),
                Amount = maps:get(<<"amount">>, Json, 0),
                
                %% Validate amount
                if
                    Amount =< 0 ->
                        Req = reply_json(Req1, 400, #{error => <<"Invalid amount">>}),
                        {ok, Req, State};
                    true ->
                        NewBalance = add_balance(UserId, Amount),
                        
                        %% Broadcast balance update to user
                        broadcast_dispatcher:send_to_user(UserId, #{
                            opcode => <<"balance_update">>,
                            user_id => UserId,
                            balance => NewBalance
                        }),
                        
                        Resp = reply_json(Req1, 200, #{
                            user_id => UserId,
                            amount_added => Amount,
                            new_balance => NewBalance
                        }),
                        {ok, Resp, State}
                end
            catch
                error:badarg ->
                    Req = reply_json(Req1, 400, #{error => <<"Invalid JSON">>}),
                    {ok, Req, State};
                error:_ ->
                    Req = reply_json(Req1, 500, #{error => <<"Internal error">>}),
                    {ok, Req, State}
            end;
        {error, missing_token} ->
            Req = reply_json(Req0, 401, #{error => <<"Missing authorization token">>}),
            {ok, Req, State};
        {error, _Reason} ->
            Req = reply_json(Req0, 401, #{error => <<"Invalid or expired token">>}),
            {ok, Req, State}
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

add_balance(UserId, Amount) ->
    F = fun() ->
        case mnesia:read(account, UserId) of
            [Account] ->
                CurrentBalance = Account#account.balance,
                NewBalance = CurrentBalance + Amount,
                UpdatedAccount = Account#account{balance = NewBalance},
                mnesia:write(UpdatedAccount),
                NewBalance;
            [] ->
                %% Account doesn't exist, create with amount
                NewAccount = #account{user_id = UserId, balance = Amount},
                mnesia:write(NewAccount),
                Amount
        end
    end,
    {atomic, NewBalance} = mnesia:transaction(F),
    NewBalance.

reply_json(Req, Status, Body) ->
    NodeName = list_to_binary(atom_to_list(node())),
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>,
          <<"x-erlang-node">> => NodeName},
        jsx:encode(Body),
        Req).
