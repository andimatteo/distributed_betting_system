-module(betting_node_mnesia).

-export([init/0, wait_for_tables/0]).
 -export([init/0, wait_for_tables/0]).

-record(account, {
    user_id,        % User ID from JWT
    balance         % Current balance (float)
}).

-record(game, {
    game_id,        % Unique game ID (reference)
    question_text,  % Question text
    opt1_text,      % Option 1 text
    opt2_text,      % Option 2 text
    result,         % Result: undefined | opt1 | opt2
    betting_open,   % Boolean
    tot_opt1,       % Total amount bet on option 1
    tot_opt2,       % Total amount bet on option 2
    created_at      % Timestamp
}).

-record(bet, {
    bet_id,         % Unique bet ID (reference)
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
    %% Stop Mnesia if running
    mnesia:stop(),
    
    %% Try to connect to other nodes asynchronously (backend pattern)
    io:format("Attempting to connect to other nodes...~n"),
    lists:foreach(fun(Node) ->
        case Node of
            N when N =:= node() -> ok;
            N -> spawn(fun() -> connect_node_loop(N, 10, 300) end)
        end
    end, AllNodes),
    
    %% Get currently connected nodes
    ConnectedNodes = [node() | nodes()],
    io:format("Connected nodes: ~p~n", [ConnectedNodes]),
    
    %% Create schema for all connected nodes
    io:format("Creating schema for nodes: ~p~n", [ConnectedNodes]),
    case mnesia:create_schema(ConnectedNodes) of
        ok -> 
            io:format("Schema created~n");
        {error, {_, {already_exists, _}}} ->
            io:format("Schema already exists~n")
    end,
    
    %% Start Mnesia
    mnesia:start(),
    
    %% Start Mnesia on remote nodes asynchronously
    lists:foreach(fun(Node) ->
        case Node of
            N when N =:= node() -> ok;
            N -> spawn(fun() -> safe_start_remote_mnesia(N) end)
        end
    end, ConnectedNodes),
    
    %% Wait a bit for remote Mnesia instances
    timer:sleep(1000),
    
    %% Wait a bit for other nodes to start their Mnesia
    timer:sleep(1000),
    
    %% Create tables if they don't exist
    case mnesia:system_info(tables) of
        [schema] ->
            io:format("Creating tables...~n"),
            create_tables(ConnectedNodes);
        Tables ->
            io:format("Tables already exist: ~p~n", [Tables])
    end,
    
    %% Wait for tables
    wait_for_tables(),
    ok.

init_worker_node() ->
    %% Connect to master node
    MasterNode = 'betting_node1@10.2.1.62',
    io:format("Connecting to master node ~p...~n", [MasterNode]),
    %% Request connection using net_kernel
    _ = net_kernel:connect_node(MasterNode),
    %% Wait briefly for the connection to establish
    timer:sleep(500),
    case lists:member(MasterNode, nodes()) of
        true ->
            io:format("Connected to master node~n"),
            
            %% Stop Mnesia if running
            mnesia:stop(),
            
            %% Start Mnesia
            mnesia:start(),
            
            %% Wait for tables to be replicated
            timer:sleep(2000),
            
            %% Add table copies if needed
            add_table_copies(),
            
            %% Wait for tables
            wait_for_tables();
        false ->
            io:format("WARNING: Master node not available, starting standalone~n"),
            mnesia:start()
    end,
    ok.

create_tables(Nodes) ->
    io:format("Creating tables with replicas on: ~p~n", [Nodes]),
    
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

add_table_copies() ->
    io:format("Adding table copies to ~p...~n", [node()]),
    
    Tables = [account, game, bet],
    lists:foreach(fun(Table) ->
        case catch mnesia:add_table_copy(Table, node(), disc_copies) of
            {atomic, ok} ->
                io:format("Added replica for table ~p~n", [Table]);
            {aborted, {already_exists, Table, _}} ->
                io:format("Replica for table ~p already exists~n", [Table]);
            {aborted, Reason} ->
                io:format("Could not add replica for table ~p: ~p~n", [Table, Reason]);
            {'EXIT', Reason} ->
                io:format("Table ~p does not exist yet: ~p~n", [Table, Reason])
        end
    end, Tables),
    ok.

wait_for_tables() ->
    Tables = [account, game, bet],
    case mnesia:wait_for_tables(Tables, 10000) of
        ok -> ok;
        {timeout, BadTables} ->
            io:format("Timeout waiting for tables: ~p~n", [BadTables]),
            ok;
        {error, Reason} ->
            io:format("Error waiting for tables: ~p~n", [Reason]),
            ok
    end.

%% Async connect loop similar to backend style
connect_node_loop(Node, 0, _Delay) ->
    case lists:member(Node, nodes()) of
        true -> ok;
        false -> ok
    end;
connect_node_loop(Node, Attempts, Delay) ->
    _ = net_kernel:connect_node(Node),
    timer:sleep(Delay),
    case lists:member(Node, nodes()) of
        true -> ok;
        false -> connect_node_loop(Node, Attempts - 1, Delay)
    end.

safe_start_remote_mnesia(Node) ->
    case lists:member(Node, nodes()) of
        true -> catch rpc:call(Node, mnesia, start, []); 
        false -> ok
    end.
