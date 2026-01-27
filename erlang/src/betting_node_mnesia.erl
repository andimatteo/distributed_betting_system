-module(betting_node_mnesia).

-export([init/0, wait_for_tables/0, next_game_id/0, next_bet_id/0]).

-record(account, {
    user_id,        % User ID from JWT
    balance         % Current balance (float)
}).

-record(counter, {
    name,           % Counter name (game_id | bet_id)
    value           % Current value (integer)
}).

-record(game, {
    game_id,        % Unique game ID (integer)
    question_text,  % Question text
    opt1_text,      % Option 1 text
    opt2_text,      % Option 2 text
    category,       % real | virtual
    result,         % Result: undefined | opt1 | opt2
    betting_open,   % Boolean
    tot_opt1,       % Total amount bet on option 1
    tot_opt2,       % Total amount bet on option 2
    created_at      % Timestamp
}).

-record(bet, {
    bet_id,         % Unique bet ID (integer)
    user_id,        % User ID
    game_id,        % Game ID
    amount,         % Bet amount (float)
    choice,         % opt1 | opt2
    odd,            % Odd at the time of betting (float)
    placed_at       % Timestamp
}).

init() ->
    io:format("Initializing Mnesia on ~p...~n", [node()]),
    
    %% Define all cluster nodes (static configuration)
    AllNodes = [
        'betting_node1@10.2.1.62',
        'betting_node2@10.2.1.27',
        'betting_node3@10.2.1.28'
    ],
    
    %% Determine if this is the first node starting
    IsFirstNode = node() == 'betting_node1@10.2.1.62',
    
    case IsFirstNode of
        true ->
            io:format("This is the master node - initializing cluster...~n"),
            init_master_node(AllNodes);
        false ->
            io:format("This is a worker node - connecting to cluster...~n"),
            init_worker_node()
    end,
    
    io:format("Mnesia initialized successfully on ~p~n", [node()]),
    ok.

init_master_node(AllNodes) ->
    %% Clean start: stop, delete and recreate schema
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    
    %% Wait for all other nodes to connect
    OtherNodes = [N || N <- AllNodes, N =/= node()],
    io:format("Waiting for all nodes to connect: ~p~n", [OtherNodes]),
    wait_for_all_nodes(OtherNodes, 120),  %% Wait up to 120 seconds
    
    %% Get all connected nodes including self
    ConnectedNodes = [node() | nodes()],
    io:format("All nodes connected: ~p~n", [ConnectedNodes]),
    
    %% Delete schema on all remote nodes first
    lists:foreach(fun(Node) ->
        case Node of
            N when N =:= node() -> ok;
            N -> 
                io:format("Deleting schema on ~p...~n", [N]),
                rpc:call(N, mnesia, stop, [], 5000),
                rpc:call(N, mnesia, delete_schema, [[N]], 5000)
        end
    end, ConnectedNodes),
    
    %% Create fresh schema for all connected nodes
    io:format("Creating schema for nodes: ~p~n", [ConnectedNodes]),
    ok = mnesia:create_schema(ConnectedNodes),
    io:format("Schema created~n"),
    
    %% Start Mnesia on master
    mnesia:start(),
    
    %% Start Mnesia on all remote nodes
    lists:foreach(fun(Node) ->
        case Node of
            N when N =:= node() -> ok;
            N -> 
                io:format("Starting Mnesia on ~p...~n", [N]),
                case rpc:call(N, mnesia, start, [], 5000) of
                    ok -> io:format("Mnesia started on ~p~n", [N]);
                    {error, Reason} -> io:format("Failed to start Mnesia on ~p: ~p~n", [N, Reason]);
                    {badrpc, Reason} -> io:format("RPC failed to ~p: ~p~n", [N, Reason])
                end
        end
    end, ConnectedNodes),
    
    %% Wait for all Mnesia instances to be ready
    timer:sleep(2000),
    
    %% Always create tables (fresh start)
    io:format("Creating tables with replicas on all nodes: ~p~n", [ConnectedNodes]),
    create_tables(ConnectedNodes),
    
    %% Wait for tables
    wait_for_tables(),
    
    %% Initialize bookmaker account
    init_bookmaker_account(),
    ok.

init_worker_node() ->
    %% Clean start: stop and delete local schema
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    
    %% Connect to master node
    MasterNode = 'betting_node1@10.2.1.62',
    io:format("Connecting to master node ~p...~n", [MasterNode]),
    
    %% Keep trying to connect to master
    wait_for_master(MasterNode, 60),
    
    case lists:member(MasterNode, nodes()) of
        true ->
            io:format("Connected to master node~n"),
            io:format("Waiting for master to initialize Mnesia...~n"),
            
            %% Wait for master to finish creating schema and starting Mnesia
            timer:sleep(5000),
            
            %% Master will create schema and start Mnesia via RPC, just wait for tables
            wait_for_tables(),
            
            %% Ensure we have local copies of all tables
            ensure_table_copies(),
            ok;
        false ->
            io:format("~n"),
            io:format("╔════════════════════════════════════════════════════════════╗~n"),
            io:format("║  ERROR: Master node not available!                         ║~n"),
            io:format("║  Please start betting_node1@10.2.1.62 first.               ║~n"),
            io:format("╚════════════════════════════════════════════════════════════╝~n"),
            io:format("~n"),
            erlang:error(master_node_unavailable)
    end.

wait_for_master(_, 0) ->
    io:format("Timeout waiting for master node~n"),
    ok;
wait_for_master(MasterNode, SecondsLeft) ->
    _ = net_kernel:connect_node(MasterNode),
    timer:sleep(1000),
    case lists:member(MasterNode, nodes()) of
        true -> ok;
        false ->
            io:format("Waiting for master node... (~p seconds remaining)~n", [SecondsLeft - 1]),
            wait_for_master(MasterNode, SecondsLeft - 1)
    end.

create_tables(Nodes) ->
    io:format("Creating tables with replicas on: ~p~n", [Nodes]),
    
    %% Create counter table for ID generation
    mnesia:create_table(counter, [
        {attributes, record_info(fields, counter)},
        {disc_copies, Nodes},
        {type, set}
    ]),
    
    %% Create account table
    mnesia:create_table(account, [
        {attributes, record_info(fields, account)},
        {disc_copies, Nodes},
        {type, set}
    ]),
    
    %% Create game table
    mnesia:create_table(game, [
        {attributes, record_info(fields, game)},
        {disc_copies, Nodes},
        {type, set},
        {index, [betting_open]}
    ]),
    
    %% Create bet table
    mnesia:create_table(bet, [
        {attributes, record_info(fields, bet)},
        {disc_copies, Nodes},
        {type, set},
        {index, [user_id, game_id]}
    ]),
    
    io:format("Tables created successfully~n"),
    ok.

wait_for_tables() ->
    Tables = [counter, account, game, bet],
    io:format("Waiting for tables: ~p~n", [Tables]),
    case mnesia:wait_for_tables(Tables, 30000) of
        ok -> 
            io:format("All tables ready~n"),
            ok;
        {timeout, BadTables} ->
            io:format("Timeout waiting for tables: ~p~n", [BadTables]),
            io:format("Retrying...~n"),
            timer:sleep(5000),
            wait_for_tables();
        {error, Reason} ->
            io:format("Error waiting for tables: ~p~n", [Reason]),
            io:format("Retrying...~n"),
            timer:sleep(5000),
            wait_for_tables()
    end.

%% Wait for all nodes to connect before proceeding
wait_for_all_nodes(NodesToWait, TimeoutSeconds) ->
    wait_for_all_nodes(NodesToWait, TimeoutSeconds, 0).

wait_for_all_nodes([], _TimeoutSeconds, _Elapsed) ->
    io:format("All nodes connected!~n"),
    ok;
wait_for_all_nodes(NodesToWait, TimeoutSeconds, Elapsed) when Elapsed >= TimeoutSeconds ->
    io:format("TIMEOUT: Still waiting for nodes: ~p~n", [NodesToWait]),
    io:format("Proceeding with connected nodes only...~n"),
    ok;
wait_for_all_nodes(NodesToWait, TimeoutSeconds, Elapsed) ->
    %% Try to connect to all missing nodes
    lists:foreach(fun(Node) ->
        _ = net_kernel:connect_node(Node)
    end, NodesToWait),
    
    timer:sleep(1000),
    
    %% Check which nodes are now connected
    Connected = nodes(),
    StillWaiting = [N || N <- NodesToWait, not lists:member(N, Connected)],
    
    case StillWaiting of
        [] ->
            io:format("All nodes connected!~n"),
            ok;
        _ ->
            RemainingTime = TimeoutSeconds - Elapsed - 1,
            io:format("Waiting for nodes ~p... (~p seconds remaining)~n", [StillWaiting, RemainingTime]),
            wait_for_all_nodes(StillWaiting, TimeoutSeconds, Elapsed + 1)
    end.

init_bookmaker_account() ->
    BookmakerMoney = application:get_env(betting_node, bookmaker_money, 0),
    BookmakerId = <<"bookmaker">>,
    F = fun() ->
        case mnesia:read(account, BookmakerId) of
            [] ->
                mnesia:write(#account{user_id = BookmakerId, balance = BookmakerMoney}),
                io:format("Bookmaker account created with balance: ~p~n", [BookmakerMoney]);
            [_] ->
                io:format("Bookmaker account already exists~n")
        end
    end,
    {atomic, _} = mnesia:transaction(F),
    ok.

%% Ensure this node has local copies of all tables
ensure_table_copies() ->
    Tables = [counter, account, game, bet],
    lists:foreach(fun(Table) ->
        case mnesia:add_table_copy(Table, node(), disc_copies) of
            {atomic, ok} ->
                io:format("Added local copy of table ~p~n", [Table]);
            {aborted, {already_exists, _, _}} ->
                io:format("Table ~p already has local copy~n", [Table]);
            {aborted, Reason} ->
                io:format("Failed to add local copy of table ~p: ~p~n", [Table, Reason])
        end
    end, Tables),
    ok.
%% Get next game ID
next_game_id() ->
    next_id(game_id).

%% Get next bet ID
next_bet_id() ->
    next_id(bet_id).

%% Generic function to get next ID from counter
next_id(CounterName) ->
    F = fun() ->
        case mnesia:read(counter, CounterName) of
            [] ->
                %% Initialize counter
                mnesia:write(#counter{name = CounterName, value = 1}),
                1;
            [#counter{value = Current}] ->
                Next = Current + 1,
                mnesia:write(#counter{name = CounterName, value = Next}),
                Next
        end
    end,
    {atomic, Id} = mnesia:transaction(F),
    Id.