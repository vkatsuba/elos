-module(elos).

%%% ============================================================================
%%% Behaviours
%%% ============================================================================

-behaviour(gen_server).

%%% ============================================================================
%%% Exports of gen_server
%%% ============================================================================

-export([
    start_link/0,
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%%% ============================================================================
%%% Exports of API
%%% ============================================================================

-export([
    start_instance/1,
    stop_instance/1
]).

%%% ============================================================================
%%% Callbacks of gen_server
%%% ============================================================================

start_link() ->
    Config = #{name := Name} = config(),
    gen_server:start_link({local, Name}, ?MODULE, Config, []).

start_link(#{name := Name} = Config) ->
    gen_server:start_link({local, Name}, ?MODULE, Config, []).

init(#{ip := IP, port := Port, recbuf := RecBuf} = Config) ->
    process_flag(trap_exit, true),
    logger_setup(Config),
    case gen_udp:open(Port, [{active, once}, {ip, IP}, binary, {recbuf, RecBuf}]) of
        {ok, Socket} ->
            {ok, Config#{socket => Socket}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, Socket, FromIP, FromPortNo, <<Level:8, LogMsg/binary>>}, State) ->
    logger:(log_level(Level))("~s", [LogMsg]),
    inet:setopts(Socket, [{active, once}]),
    gen_udp:send(Socket, FromIP, FromPortNo, <<Level:8>>),
    {noreply, State};
handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{socket := Socket}) ->
    gen_udp:close(Socket),
    ok.

%%% ============================================================================
%%% API
%%% ============================================================================

start_instance(Config) ->
    elos_sup:start_instance(Config).

stop_instance(ID) ->
    elos_sup:stop_instance(ID).

%%% ============================================================================
%%% Internal functions
%%% ============================================================================

% Convert command to log level: https://erlang.org/doc/apps/kernel/logger_chapter.html#log-level
log_level(0) ->
    emergency;
log_level(1) ->
    alert;
log_level(2) ->
    critical;
log_level(3) ->
    error;
log_level(4) ->
    warning;
log_level(5) ->
    notice;
log_level(6) ->
    info;
log_level(7) ->
    debug;
log_level(Cmd) ->
    exit(lists:flatten(io_lib:format("Cannot convert log level ~p", [Cmd]))).

% Get config
config() ->
    IP = {A, B, C, D} = application:get_env(elos, ip, {127, 0, 0, 1}),
    Port = application:get_env(elos, port, 8181),
    Name = io_lib:format("~b.~b.~b.~b:~b", [A, B, C, D, Port]),
    #{
        name => list_to_atom(Name),
        ip => IP,
        port => Port,
        file => application:get_env(elos, file, "log/" ++ Name ++ ".log"),
        level => application:get_env(elos, level, all),
        recbuf => application:get_env(elos, recbuf, 8192)
    }.

% Set log config with adding and updating of logger
logger_setup(#{name := Name, file := File, level := Level}) ->
    ConfigLogger = #{config => #{file => File}, level => Level},
    logger:add_handler(Name, logger_disk_log_h, ConfigLogger),
    logger:update_primary_config(#{level => all}).
