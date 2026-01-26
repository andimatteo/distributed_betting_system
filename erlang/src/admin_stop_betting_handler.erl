-module(admin_stop_betting_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-record(game, {game_id, question_text, opt1_text, opt2_text, result, betting_open, tot_opt1, tot_opt2, created_at}).

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

        #{<<"game_id">> := GameIdStr} = jsx:decode(Body, [return_maps]),
        GameId = string_to_ref(GameIdStr),

        ok = stop_betting(GameId),
        Resp = reply_json(Req1, 200, #{
            message => <<"Betting stopped for game">>,
            game_id => GameIdStr
        }),
        {ok, Resp, State}
    catch
        error:game_not_found ->
            {ok, reply_json(Req0, 404, #{error => <<"Game not found">>}), State};
        error:_ ->
            {ok, reply_json(Req0, 500, #{error => <<"internal_error">>}), State}
    end.

stop_betting(GameId) ->
    F = fun() ->
        case mnesia:read(game, GameId) of
            [Game] ->
                UpdatedGame = Game#game{betting_open = false},
                mnesia:write(UpdatedGame),
                ok;
            [] ->
                mnesia:abort(game_not_found)
        end
    end,

    case mnesia:transaction(F) of
        {atomic, ok} ->
            %% Broadcast betting closed to all clients
            spawn(fun() ->
                GameIdStr = ref_to_string(GameId),
                broadcast_dispatcher:broadcast({betting_closed, GameIdStr})
            end),
            ok;
        {aborted, game_not_found} -> erlang:error(game_not_found)
    end.

ref_to_string(Ref) when is_reference(Ref) ->
    list_to_binary(erlang:ref_to_list(Ref));
ref_to_string(Other) ->
    Other.

string_to_ref(RefStr) when is_binary(RefStr) ->
    erlang:list_to_ref(binary_to_list(RefStr)).

reply_json(Req, Status, Body) ->
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body),
        Req).
