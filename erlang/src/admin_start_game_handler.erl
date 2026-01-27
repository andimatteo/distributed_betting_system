-module(admin_start_game_handler).

-behaviour(cowboy_handler).

-export([init/2]).


-record(game, {game_id, question_text, opt1_text, opt2_text, category, result, betting_open, tot_opt1, tot_opt2, created_at}).
-record(bet, {bet_id, user_id, game_id, amount, choice, odd, placed_at}).
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
        {ok, _AdminId} = jwt_helper:validate_admin_jwt(Req0),
        {ok, Body, Req1} = cowboy_req:read_body(Req0),

        #{<<"game_id">> := GameIdInt, <<"result">> := ResultBin} = jsx:decode(Body, [return_maps]),
        Result = case ResultBin of
            <<"opt1">> -> opt1;
            <<"opt2">> -> opt2;
            _ -> erlang:error(invalid_result)
        end,

        GameId = GameIdInt,
        {WinnersCount, TotalPaid} = finish_game(GameId, Result),

        Resp = reply_json(Req1, 200, #{
            message => <<"Game finished and payouts processed">>,
            game_id => GameIdInt,
            result => result_to_binary(Result),
            winners_count => WinnersCount,
            total_paid => TotalPaid
        }),
        {ok, Resp, State}
    catch
        error:game_not_found ->
            {ok, reply_json(Req0, 404, #{error => <<"Game not found">>}), State};
        error:invalid_result ->
            {ok, reply_json(Req0, 400, #{error => <<"Invalid result. Must be opt1 or opt2">>}), State};
        error:_ ->
            {ok, reply_json(Req0, 500, #{error => <<"internal_error">>}), State}
    end.

finish_game(GameId, Result) ->
    F = fun() ->
        case mnesia:read(game, GameId) of
            [Game = #game{tot_opt1 = TotOpt1, tot_opt2 = TotOpt2}] ->
                UpdatedGame = Game#game{betting_open = false, result = Result},
                mnesia:write(UpdatedGame),

                TotalPool = TotOpt1 + TotOpt2,
                WinningBets = get_winning_bets(GameId, Result),

                {WinnersCount, TotalPaid, BalanceUpdates} = case Result of
                    opt1 when TotOpt1 > 0 ->
                        pay_winners(WinningBets);
                    opt2 when TotOpt2 > 0 ->
                        pay_winners(WinningBets);
                    _ ->
                        refund_all_bets(GameId)
                end,

                %% Update bookmaker balance: profit = total_pool - total_paid
                update_bookmaker_balance(TotalPool - TotalPaid),

                {WinnersCount, TotalPaid, BalanceUpdates};
            [] ->
                mnesia:abort(game_not_found)
        end
    end,

    case mnesia:transaction(F) of
        {atomic, {WinnersCount, TotalPaid, BalanceUpdates}} ->
            %% Broadcast game result and balance updates
            spawn(fun() ->
                ResultBin = result_to_binary(Result),
                broadcast_dispatcher:broadcast({game_result, GameId, ResultBin, WinnersCount, TotalPaid}),
                %% Send individual balance updates to winners
                lists:foreach(fun({UserId, NewBalance}) ->
                    broadcast_dispatcher:broadcast({balance_update, UserId, NewBalance})
                end, BalanceUpdates)
            end),
            {WinnersCount, TotalPaid};
        {aborted, game_not_found} ->
            erlang:error(game_not_found)
    end.

get_winning_bets(GameId, WinningChoice) ->
    mnesia:select(bet, [{#bet{game_id = GameId, choice = WinningChoice, _ = '_'}, [], ['$_']}]).

pay_winners(WinningBets) ->
    %% Pay winners based on their stored odd at time of betting
    lists:foldl(fun(Bet, {Count, TotalPaid, Updates}) ->
        #bet{user_id = UserId, amount = BetAmount, odd = Odd} = Bet,
        
        %% Calculate payout using the odd stored in the bet: payout = amount * odd
        Payout = BetAmount * Odd,
        
        %% Update user balance
        case mnesia:read(account, UserId) of
            [Account = #account{balance = Balance}] ->
                NewBalance = Balance + Payout,
                mnesia:write(Account#account{balance = NewBalance}),
                {Count + 1, TotalPaid + Payout, [{UserId, NewBalance} | Updates]};
            [] ->
                {Count, TotalPaid, Updates}
        end
    end, {0, 0.0, []}, WinningBets).

update_bookmaker_balance(Amount) ->
    BookmakerId = <<"bookmaker">>,
    case mnesia:read(account, BookmakerId) of
        [Account = #account{balance = Balance}] ->
            NewBalance = Balance + Amount,
            mnesia:write(Account#account{balance = NewBalance});
        [] ->
            %% Should not happen if initialized properly
            BookmakerMoney = application:get_env(betting_node, bookmaker_money, 10000),
            mnesia:write(#account{user_id = BookmakerId, balance = BookmakerMoney + Amount})
    end.

refund_all_bets(GameId) ->
    AllBets = mnesia:select(bet, [{#bet{game_id = GameId, _ = '_'}, [], ['$_']}]),
    
    lists:foldl(fun(Bet, {Count, TotalRefunded, Updates}) ->
        #bet{user_id = UserId, amount = Amount} = Bet,
        
        case mnesia:read(account, UserId) of
            [Account = #account{balance = Balance}] ->
                NewBalance = Balance + Amount,
                mnesia:write(Account#account{balance = NewBalance}),
                {Count + 1, TotalRefunded + Amount, [{UserId, NewBalance} | Updates]};
            [] ->
                {Count, TotalRefunded, Updates}
        end
    end, {0, 0.0, []}, AllBets).

result_to_binary(opt1) -> <<"opt1">>;
result_to_binary(opt2) -> <<"opt2">>;
result_to_binary(Other) -> Other.

reply_json(Req, Status, Body) ->
    NodeName = list_to_binary(atom_to_list(node())),
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>,
          <<"x-erlang-node">> => NodeName},
        jsx:encode(Body),
        Req).
