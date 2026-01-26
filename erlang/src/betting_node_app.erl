-module(betting_node_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Starting Betting Node Application...~n"),
    
    %% Register the node name
    register(?MODULE, self()),
    
    %% Initialize or join Mnesia cluster
    betting_node_mnesia:init(),
    
    %% Start the supervisor (which will start cowboy_listener)
    {ok, Pid} = betting_node_sup:start_link(),
    
    io:format("Betting Node Application started successfully on ~p~n", [node()]),
    {ok, Pid}.

stop(_State) ->
    io:format("Stopping Betting Node Application...~n"),
    ok.
