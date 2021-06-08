%%%-------------------------------------------------------------------
%% @doc elos top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(elos_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    start_instance/1,
    stop_instance/1,
    init/1
]).

-define(SERVER, ?MODULE).

-define(CHILD(ID, Args), #{
    id => ID,
    start => {elos, start_link, Args},
    type => worker,
    restart => permanent,
    shutdown => 5000
}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_instance(#{name := Name} = Config) ->
    supervisor:start_child(?SERVER, ?CHILD(Name, [Config])).

stop_instance(ID) ->
    supervisor:terminate_child(?SERVER, ID).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 1
    },
    {ok, {SupFlags, [?CHILD(elos, [])]}}.
