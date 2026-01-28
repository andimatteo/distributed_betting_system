-module(broadcast_dispatcher).

-export([start/0, broadcast/1, broadcast_admin/1]).

start() ->
    global:register_name({dispatcher, node(), self()}, self()),
    io:format("Broadcast dispatcher started on ~p with PID ~p~n", [node(), self()]),
    loop().

%% Public API to send a broadcast message
broadcast(Msg) ->
    RegisteredPids = global:registered_names(),
    lists:foreach(fun(RegisteredPid) ->
        case RegisteredPid of
            {dispatcher, _, Pid} ->
                Pid ! Msg;
            _ ->
                ok
        end
    end, RegisteredPids).

%% Public API to send message to admin users only
broadcast_admin(Msg) ->
    RegisteredPids = global:registered_names(),
    lists:foreach(fun(RegisteredPid) ->
        case RegisteredPid of
            {dispatcher, _, Pid} ->
                Pid ! {admin_only, Msg};
            _ ->
                ok
        end
    end, RegisteredPids).

loop() ->
    receive 
        {update_odds, GameId, Odd1, Odd2, CapOpt1, CapOpt2} ->
            broadcast_to_websockets(jsx:encode(#{
                <<"opcode">> => <<"odds_update">>,
                <<"game_id">> => GameId,
                <<"odd1">> => Odd1,
                <<"odd2">> => Odd2,
                <<"cap_opt1">> => CapOpt1,
                <<"cap_opt2">> => CapOpt2
            }));
        
        {new_game, GameId, QuestionText, Opt1Text, Opt2Text, Category} ->
            %% Compute initial odds and caps (with no bets, virtual_init_bet handles it)
            {Odd1, Odd2} = odds_calculator:calculate_odds(0, 0, [], []),
            {CapOpt1, CapOpt2} = odds_calculator:calculate_caps(0, 0, [], []),
            CategoryBin = case Category of
                real -> <<"real">>;
                virtual -> <<"virtual">>;
                _ -> <<"real">>
            end,
            broadcast_to_websockets(jsx:encode(#{
                <<"opcode">> => <<"new_game">>,
                <<"game_id">> => GameId,
                <<"question_text">> => QuestionText,
                <<"opt1_text">> => Opt1Text,
                <<"opt2_text">> => Opt2Text,
                <<"category">> => CategoryBin,
                <<"betting_open">> => true,
                <<"odd1">> => Odd1,
                <<"odd2">> => Odd2,
                <<"cap_opt1">> => CapOpt1,
                <<"cap_opt2">> => CapOpt2
            }));
        
        {betting_closed, GameId} ->
            broadcast_to_websockets(jsx:encode(#{
                <<"opcode">> => <<"betting_closed">>,
                <<"game_id">> => GameId
            }));
        
        {game_result, GameId, Result, WinnersCount, TotalPaid} ->
            broadcast_to_websockets(jsx:encode(#{
                <<"opcode">> => <<"game_result">>,
                <<"game_id">> => GameId,
                <<"result">> => Result,
                <<"winners_count">> => WinnersCount,
                <<"total_paid">> => TotalPaid
            }));
        
        {balance_update, UserId, NewBalance} ->
            %% Send balance update only to the specific user's WebSocket
            broadcast_to_user(UserId, jsx:encode(#{
                <<"opcode">> => <<"balance_update">>,
                <<"user_id">> => UserId,
                <<"balance">> => NewBalance
            }));
        
        {balance_update, UserId, NewBalance, GameId} ->
            %% Send balance update with game_id (for game result payouts)
            broadcast_to_user(UserId, jsx:encode(#{
                <<"opcode">> => <<"balance_update">>,
                <<"user_id">> => UserId,
                <<"balance">> => NewBalance,
                <<"game_id">> => GameId
            }));
        
        {bet_placed, UserId, GameId, Amount, Choice, Odd} ->
            %% Confirm bet to specific user
            ChoiceBin = case Choice of
                opt1 -> <<"opt1">>;
                opt2 -> <<"opt2">>;
                _ -> <<"unknown">>
            end,
            broadcast_to_user(UserId, jsx:encode(#{
                <<"opcode">> => <<"bet_confirmed">>,
                <<"game_id">> => GameId,
                <<"amount">> => Amount,
                <<"choice">> => ChoiceBin,
                <<"odd">> => Odd
            }));
        
        {admin_only, {profit_update, Profit}} ->
            %% Send profit update only to admin WebSocket connections
            broadcast_to_admins(jsx:encode(#{
                <<"opcode">> => <<"profit_update">>,
                <<"profit">> => Profit
            }));
        
        _ ->
            ok
    end,
    loop().

broadcast_to_websockets(Msg) ->
    RegisteredPids = global:registered_names(),
    lists:foreach(fun(RegisteredPid) ->
        case RegisteredPid of
            {ws, Node, Pid} when Node =:= node() ->
                Pid ! {broadcast, Msg};
            {ws_user, Node, _UserId, Pid} when Node =:= node() ->
                Pid ! {broadcast, Msg};
            {ws_admin, Node, _UserId, Pid} when Node =:= node() ->
                Pid ! {broadcast, Msg};
            _ ->
                ok
        end
    end, RegisteredPids).

broadcast_to_user(UserId, Msg) ->
    RegisteredPids = global:registered_names(),
    lists:foreach(fun(RegisteredPid) ->
        case RegisteredPid of
            {ws_user, Node, UID, Pid} when Node =:= node(), UID =:= UserId ->
                Pid ! {broadcast, Msg};
            {ws_admin, Node, UID, Pid} when Node =:= node(), UID =:= UserId ->
                Pid ! {broadcast, Msg};
            _ ->
                ok
        end
    end, RegisteredPids).

broadcast_to_admins(Msg) ->
    RegisteredPids = global:registered_names(),
    lists:foreach(fun(RegisteredPid) ->
        case RegisteredPid of
            {ws_admin, Node, _UserId, Pid} when Node =:= node() ->
                Pid ! {broadcast, Msg};
            _ ->
                ok
        end
    end, RegisteredPids).
