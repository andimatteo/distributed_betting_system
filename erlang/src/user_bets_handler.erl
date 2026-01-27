-module(user_bets_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-record(account, {user_id, balance}).
-record(game, {game_id, question_text, opt1_text, opt2_text, category, result, betting_open, tot_opt1, tot_opt2, created_at}).
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
        {ok, UserId, _IsAdmin} ->
            %% Check if game_id is provided in the path
            case cowboy_req:binding(game_id, Req0) of
                undefined ->
                    %% Get all bets for the user
                    {ok, Bets} = get_user_bets(UserId),
                    Req = reply_json(Req0, 200, #{bets => Bets}),
                    {ok, Req, State};
                GameIdBin ->
                    %% Get bets for a specific game
                    GameId = binary_to_integer(GameIdBin),
                    {ok, Bets} = get_user_bets_for_game(UserId, GameId),
                    Req = reply_json(Req0, 200, #{bets => Bets}),
                    {ok, Req, State}
            end;
        {error, missing_token} ->
            Req = reply_json(Req0, 401, #{error => <<"Missing authorization token">>}),
            {ok, Req, State};
        {error, _Reason} ->
            Req = reply_json(Req0, 401, #{error => <<"Invalid or expired token">>}),
            {ok, Req, State}
    end.

%% Get all bets for a user
get_user_bets(UserId) ->
    F = fun() ->
        %% Get all bets for this user
        Bets = mnesia:select(bet, [{#bet{user_id = UserId, _ = '_'}, [], ['$_']}]),
        %% For each bet, get the game info
        lists:map(fun(Bet) -> bet_with_game_info(Bet) end, Bets)
    end,
    {atomic, BetsWithInfo} = mnesia:transaction(F),
    {ok, BetsWithInfo}.

%% Get bets for a specific game
get_user_bets_for_game(UserId, GameId) ->
    F = fun() ->
        %% Get all bets for this user and game
        Bets = mnesia:select(bet, [{#bet{user_id = UserId, game_id = GameId, _ = '_'}, [], ['$_']}]),
        %% For each bet, get the game info
        lists:map(fun(Bet) -> bet_with_game_info(Bet) end, Bets)
    end,
    {atomic, BetsWithInfo} = mnesia:transaction(F),
    {ok, BetsWithInfo}.

%% Enrich bet with game information
bet_with_game_info(Bet) ->
    GameId = Bet#bet.game_id,
    [Game] = mnesia:read(game, GameId),
    
    %% Calculate payout
    {Won, Payout} = case Game#game.result of
        undefined ->
            {null, null};
        Result when Result =:= Bet#bet.choice ->
            %% User won
            PayoutAmount = Bet#bet.amount * Bet#bet.odd,
            {true, PayoutAmount};
        _ ->
            %% User lost
            {false, 0.0}
    end,
    
    #{
        bet_id => Bet#bet.bet_id,
        user_id => Bet#bet.user_id,
        game_id => GameId,
        amount => Bet#bet.amount,
        choice => choice_to_binary(Bet#bet.choice),
        odd => Bet#bet.odd,
        placed_at => Bet#bet.placed_at,
        %% Game info
        game_question => Game#game.question_text,
        game_category => category_to_binary(Game#game.category),
        opt1_text => Game#game.opt1_text,
        opt2_text => Game#game.opt2_text,
        game_result => result_to_binary(Game#game.result),
        betting_open => Game#game.betting_open,
        %% Outcome info
        won => Won,
        payout => Payout
    }.

choice_to_binary(opt1) -> <<"opt1">>;
choice_to_binary(opt2) -> <<"opt2">>;
choice_to_binary(Other) -> Other.

category_to_binary(real) -> <<"real">>;
category_to_binary(virtual) -> <<"virtual">>;
category_to_binary(_) -> <<"real">>.

result_to_binary(undefined) -> null;
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
