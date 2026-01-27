-module(admin_game_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-record(game, {game_id, question_text, opt1_text, opt2_text, category, result, betting_open, tot_opt1, tot_opt2, created_at}).

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
        #{
            <<"question_text">> := QuestionText,
            <<"opt1_text">> := Opt1Text,
            <<"opt2_text">> := Opt2Text,
            <<"category">> := CategoryBin
        } = jsx:decode(Body, [return_maps]),
        
        Category = case CategoryBin of
            <<"real">> -> real;
            <<"virtual">> -> virtual;
            _ -> erlang:error(invalid_category)
        end,

        GameId = create_game(QuestionText, Opt1Text, Opt2Text, Category),
        Resp = reply_json(Req1, 201, #{
            message => <<"Game created successfully">>,
            game_id => GameId
        }),
        {ok, Resp, State}
    catch
        error:_ ->
            {ok, reply_json(Req0, 500, #{error => <<"internal_error">>}), State}
    end.

create_game(QuestionText, Opt1Text, Opt2Text, Category) ->
    GameId = betting_node_mnesia:next_game_id(),
    Game = #game{
        game_id = GameId,
        question_text = QuestionText,
        opt1_text = Opt1Text,
        opt2_text = Opt2Text,
        category = Category,
        result = undefined,
        betting_open = true,
        tot_opt1 = 0.0,
        tot_opt2 = 0.0,
        created_at = erlang:system_time(second)
    },

    F = fun() ->
        mnesia:write(Game)
    end,

    {atomic, ok} = mnesia:transaction(F),

    %% Broadcast new game to all clients
    spawn(fun() ->
        broadcast_dispatcher:broadcast({new_game, GameId, QuestionText, Opt1Text, Opt2Text, Category})
    end),

    GameId.

reply_json(Req, Status, Body) ->
    NodeName = list_to_binary(atom_to_list(node())),
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>,
          <<"x-erlang-node">> => NodeName},
        jsx:encode(Body),
        Req).
