-module(betting_node_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    
    CowboyListener = #{
        id => cowboy_listener,
        start => {cowboy_listener, start_link, []},
        restart => permanent,
        shutdown => brutal_kill
    },
    
    ChildSpecs = [CowboyListener],
    
    {ok, {SupFlags, ChildSpecs}}.
