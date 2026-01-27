-module(odds_calculator).

-export([
    calculate_odds/4,
    calculate_caps/4,
    calculate_potential_loss/2
]).

-record(bet, {bet_id, user_id, game_id, amount, choice, odd, placed_at}).

%% Calculate current odds for both options
%% calculate_odds(TotOpt1, TotOpt2, Bets1, Bets2) -> {Odd1, Odd2}
calculate_odds(TotOpt1, TotOpt2, _Bets1, _Bets2) ->
    %% Use virtual_init_bet from sys.config for odds calculation
    VirtualInitBet = application:get_env(betting_node, virtual_init_bet, 100),
    Commission = application:get_env(betting_node, commission_percentage, 0.5),
    
    %% odd1(t+1) = 1 + (tot_opt2(t)+v)/(tot_opt1(t)+v)*(1-commission_percentage)
    Odd1 = 1.0 + ((TotOpt2 + VirtualInitBet) / (TotOpt1 + VirtualInitBet)) * (1 - Commission),
    
    %% odd2(t+1) = 1 + (tot_opt1(t)+v)/(tot_opt2(t)+v)*(1-commission_percentage)
    Odd2 = 1.0 + ((TotOpt1 + VirtualInitBet) / (TotOpt2 + VirtualInitBet)) * (1 - Commission),
    
    {Odd1, Odd2}.

%% Calculate betting caps for both options
%% calculate_caps(TotOpt1, TotOpt2, Bets1, Bets2) -> {CapOpt1, CapOpt2}
calculate_caps(TotOpt1, TotOpt2, Bets1, Bets2) ->
    %% Use margin from sys.config for cap calculation
    Margin = application:get_env(betting_node, margin, 50),
    
    %% Calculate potential losses
    Loss1 = calculate_potential_loss(Bets1, opt1),
    Loss2 = calculate_potential_loss(Bets2, opt2),
    
    %% Calculate odds for cap calculation
    {Odd1, Odd2} = calculate_odds(TotOpt1, TotOpt2, Bets1, Bets2),
    
    %% cap_opt1(t+1) = max(10, (tot_opt1(t) + tot_opt2(t) + m - loss1(t))/(odd1(t)-1))
    CapOpt1 = max(10.0, (TotOpt1 + TotOpt2 + Margin - Loss1) / max(Odd1 - 1, 0.01)),
    
    %% cap_opt2(t+1) = max(10, (tot_opt1(t) + tot_opt2(t) + m - loss2(t))/(odd2(t)-1))
    CapOpt2 = max(10.0, (TotOpt1 + TotOpt2 + Margin - Loss2) / max(Odd2 - 1, 0.01)),
    
    {CapOpt1, CapOpt2}.

%% Calculate potential loss for a given option
%% loss1(t) = sum(bet1(i) * odd1(i)) for all previous bets on opt1
calculate_potential_loss(Bets, _Choice) ->
    lists:foldl(fun(Bet, Acc) ->
        #bet{amount = Amount, odd = Odd} = Bet,
        Acc + (Amount * Odd)
    end, 0.0, Bets).
