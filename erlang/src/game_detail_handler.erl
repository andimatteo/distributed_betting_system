-module(game_detail_handler).

-behaviour(cowboy_handler).

-export([init/2]).


-record(game, {game_id, question_text, opt1_text, opt2_text, result, betting_open, tot_opt1, tot_opt2, created_at}).
-record(bet, {bet_id, user_id, game_id, amount, choice, odd, placed_at}).

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
    case jwt_helper:validate_jwt(Req0) of
        {ok, _UserId, _IsAdmin} ->
            GameIdStr = cowboy_req:binding(game_id, Req0),
            GameId = string_to_ref(GameIdStr),
            {ok, Game} = get_game(GameId),
            Req = reply_json(Req0, 200, Game),
            {ok, Req, State};
        {error, missing_token} ->
            Req = reply_json(Req0, 401, #{error => <<"Missing authorization token">>}),
            {ok, Req, State};
        {error, _Reason} ->
            Req = reply_json(Req0, 401, #{error => <<"Invalid or expired token">>}),
            {ok, Req, State}
    end.

get_game(GameId) ->
    F = fun() ->
        [Game] = mnesia:read(game, GameId),
        %% Calculate odds and caps if betting is open
        {Odd1, Odd2, CapOpt1, CapOpt2} = case Game#game.betting_open of
            true ->
                AllBets = mnesia:select(bet, [{#bet{game_id = GameId, _ = '_'}, [], ['$_']}]),
                Bets1 = [B || B <- AllBets, element(5, B) =:= opt1],
                Bets2 = [B || B <- AllBets, element(5, B) =:= opt2],
                TotOpt1 = Game#game.tot_opt1,
                TotOpt2 = Game#game.tot_opt2,
                {O1, O2} = odds_calculator:calculate_odds(TotOpt1, TotOpt2, Bets1, Bets2),
                {C1, C2} = odds_calculator:calculate_caps(TotOpt1, TotOpt2, Bets1, Bets2),
                {O1, O2, C1, C2};
            false ->
                {null, null, null, null}
        end,
        {Game, Odd1, Odd2, CapOpt1, CapOpt2}
    end,
    {atomic, {Game, Odd1, Odd2, CapOpt1, CapOpt2}} = mnesia:transaction(F),
    {ok, game_to_map(Game, Odd1, Odd2, CapOpt1, CapOpt2)}.

game_to_map(#game{
    game_id = GameId,
    question_text = Question,
    opt1_text = Opt1,
    opt2_text = Opt2,
    result = Result,
    betting_open = BettingOpen,
    tot_opt1 = TotOpt1,
    tot_opt2 = TotOpt2,
    created_at = CreatedAt
}, Odd1, Odd2, CapOpt1, CapOpt2) ->
    #{
        game_id => ref_to_string(GameId),
        question_text => Question,
        opt1_text => Opt1,
        opt2_text => Opt2,
        result => result_to_binary(Result),
        betting_open => BettingOpen,
        tot_opt1 => TotOpt1,
        tot_opt2 => TotOpt2,
        odd1 => Odd1,
        odd2 => Odd2,
        cap_opt1 => CapOpt1,
        cap_opt2 => CapOpt2,
        created_at => CreatedAt
    }.

string_to_ref(RefStr) when is_binary(RefStr) ->
    erlang:list_to_ref(binary_to_list(RefStr)).

ref_to_string(Ref) when is_reference(Ref) ->
    list_to_binary(erlang:ref_to_list(Ref));
ref_to_string(Other) ->
    Other.

result_to_binary(undefined) -> null;
result_to_binary(opt1) -> <<"opt1">>;
result_to_binary(opt2) -> <<"opt2">>;
result_to_binary(Other) -> Other.

reply_json(Req, Status, Body) ->
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body),
        Req).
