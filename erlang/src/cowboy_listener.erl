-module(cowboy_listener).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    io:format("Starting Cowboy HTTP listener...~n"),
    
    %% Get port from environment or use default
    Port = application:get_env(betting_node, http_port, 8080),
    
    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            %% WebSocket endpoint for real-time updates
            {"/ws", websocket_handler, []},
            
            %% User endpoints
            {"/api/games", games_handler, []},
            {"/api/games/:game_id", game_detail_handler, []},
            {"/api/bet", bet_handler, []},
            {"/api/balance", balance_handler, []},
            
            %% Admin endpoints
            {"/api/admin/game", admin_game_handler, []},
            {"/api/admin/start_game", admin_start_game_handler, []},
            {"/api/admin/stop_betting", admin_stop_betting_handler, []}
        ]}
    ]),
    
    %% Start Cowboy
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("Cowboy HTTP listener started on port ~p~n", [Port]),
    
    %% Start broadcast dispatcher
    spawn(fun() -> broadcast_dispatcher:start() end),
    
    {ok, []}.

handle_call(Req, _, State) ->
    {reply, Req, State}.

handle_cast(_, State) ->
    {noreply, State}.
