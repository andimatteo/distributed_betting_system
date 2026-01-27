-module(bet_handler).

-behaviour(cowboy_handler).

-export([init/2]).


-record(account, {user_id, balance}).
-record(game, {game_id, question_text, opt1_text, opt2_text, category, result, betting_open, tot_opt1, tot_opt2, created_at}).
-record(bet, {bet_id, user_id, game_id, amount, choice, odd, placed_at}).

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
    case jwt_helper:validate_jwt(Req0) of
        {ok, UserId, _IsAdmin} ->
            handle_bet_placement(Req0, UserId, State);
        {error, missing_token} ->
            Req = reply_json(Req0, 401, #{error => <<"Missing authorization token">>}),
            {ok, Req, State};
        {error, _Reason} ->
            Req = reply_json(Req0, 401, #{error => <<"Invalid or expired token">>}),
            {ok, Req, State}
    end.

handle_bet_placement(Req0, UserId, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    try
        {GameIdInt, Amount, Choice} = parse_bet_request(Body),
        GameId = GameIdInt,
        {BetId, GId, NewOdd1, NewOdd2, NewCapOpt1, NewCapOpt2, NewBalance, BetOdd} = place_bet(UserId, GameId, Amount, Choice),
        
        %% Broadcast odds update to all clients
        spawn(fun() -> 
            broadcast_dispatcher:broadcast({update_odds, GId, NewOdd1, NewOdd2, NewCapOpt1, NewCapOpt2})
        end),
        
        %% Broadcast balance update to the specific user
        spawn(fun() ->
            broadcast_dispatcher:broadcast({balance_update, UserId, NewBalance})
        end),
        
        %% Broadcast bet placed event
        spawn(fun() ->
            broadcast_dispatcher:broadcast({bet_placed, UserId, GId, Amount, Choice, BetOdd})
        end),
        
        {ok, reply_json(Req1, 201, #{
            message => <<"Bet placed successfully">>,
            bet_id => BetId,
            new_balance => NewBalance,
            bet_odd => BetOdd,
            new_odd1 => NewOdd1,
            new_odd2 => NewOdd2,
            new_cap_opt1 => NewCapOpt1,
            new_cap_opt2 => NewCapOpt2
        }), State}
    catch
        throw:{amount_exceeds_cap, Cap} ->
            {ok, reply_json(Req1, 400, #{
                error => <<"Amount exceeds cap">>,
                max_cap => Cap
            }), State};
        throw:insufficient_balance ->
            {ok, reply_json(Req1, 400, #{error => <<"Insufficient balance">>}), State};
        throw:betting_closed ->
            {ok, reply_json(Req1, 400, #{error => <<"Betting is closed">>}), State};
        error:game_not_found ->
            {ok, reply_json(Req1, 404, #{error => <<"Game not found">>}), State};
        error:_ ->
            {ok, reply_json(Req1, 500, #{error => <<"internal_error">>}), State}
    end.

parse_bet_request(Body) ->
    #{<<"game_id">> := GameId,
      <<"amount">> := Amount,
      <<"choice">> := ChoiceBin} = jsx:decode(Body, [return_maps]),
    
    Choice = case ChoiceBin of
        <<"opt1">> -> opt1;
        <<"opt2">> -> opt2
    end,
    
    true = Amount > 0,
    {GameId, Amount, Choice}.

place_bet(UserId, GameId, Amount, Choice) when Amount > 0 ->
    F = fun() ->
        [Game] = mnesia:read(game, GameId),
        true = Game#game.betting_open,
        undefined = Game#game.result,
        
        AllBets = mnesia:select(bet, [{#bet{game_id = GameId, _ = '_'}, [], ['$_']}]),
        Bets1 = [B || B <- AllBets, B#bet.choice =:= opt1],
        Bets2 = [B || B <- AllBets, B#bet.choice =:= opt2],
        
        TotOpt1 = Game#game.tot_opt1,
        TotOpt2 = Game#game.tot_opt2,
        {Odd1, Odd2} = odds_calculator:calculate_odds(TotOpt1, TotOpt2, Bets1, Bets2),
        {CapOpt1, CapOpt2} = odds_calculator:calculate_caps(TotOpt1, TotOpt2, Bets1, Bets2),
        
        {CurrentOdd, CurrentCap} = case Choice of
            opt1 -> {Odd1, CapOpt1};
            opt2 -> {Odd2, CapOpt2}
        end,
        
        Amount =< CurrentCap orelse throw({amount_exceeds_cap, CurrentCap}),
        
        [Account] = mnesia:read(account, UserId),
        Balance = Account#account.balance,
        Balance >= Amount orelse throw(insufficient_balance),
        
        NewBalance = Balance - Amount,
        mnesia:write(Account#account{balance = NewBalance}),
        
        UpdatedGame = case Choice of
            opt1 -> Game#game{tot_opt1 = Game#game.tot_opt1 + Amount};
            opt2 -> Game#game{tot_opt2 = Game#game.tot_opt2 + Amount}
        end,
        mnesia:write(UpdatedGame),
        
        BetId = betting_node_mnesia:next_bet_id(),
        Bet = #bet{
            bet_id = BetId,
            user_id = UserId,
            game_id = GameId,
            amount = Amount,
            choice = Choice,
            odd = CurrentOdd,
            placed_at = erlang:system_time(second)
        },
        mnesia:write(Bet),
        
        NewTotOpt1 = UpdatedGame#game.tot_opt1,
        NewTotOpt2 = UpdatedGame#game.tot_opt2,
        NewBets1 = case Choice of
            opt1 -> Bets1 ++ [Bet];
            _ -> Bets1
        end,
        NewBets2 = case Choice of
            opt2 -> Bets2 ++ [Bet];
            _ -> Bets2
        end,
        {NewOdd1, NewOdd2} = odds_calculator:calculate_odds(NewTotOpt1, NewTotOpt2, NewBets1, NewBets2),
        {NewCapOpt1, NewCapOpt2} = odds_calculator:calculate_caps(NewTotOpt1, NewTotOpt2, NewBets1, NewBets2),
        
        {BetId, GameId, NewOdd1, NewOdd2, NewCapOpt1, NewCapOpt2, NewBalance, CurrentOdd}
    end,
    
    {atomic, Result} = mnesia:transaction(F),
    Result.

reply_json(Req, Status, Body) ->
    NodeName = list_to_binary(atom_to_list(node())),
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>,
          <<"x-erlang-node">> => NodeName},
        jsx:encode(Body),
        Req).
