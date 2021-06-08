-module(elos_SUITE).

%%% ============================================================================
%%% Common Tests Callbacks Exports
%%% ============================================================================

-export([
    all/0,
    groups/0,
    init_per_testcase/2,
    end_per_testcase/1,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2
]).

%%% ============================================================================
%%% Tests case exports
%%% ============================================================================

-export([
    elos_echo/1,
    elos_log_files/1,
    elos_start_new_instance/1
]).

%%% ============================================================================
%%% Specification
%%% ============================================================================

-type config() :: [{atom(), term()}].

%%% ============================================================================
%%% Macros
%%% ============================================================================

-define(APP_CONFIG, [
    {ip, {127, 0, 0, 1}},
    {port, 8181},
    {file, "log/127.0.0.1:8181.log"},
    {level, alert},
    {recbuf, 8192}
]).

%%% ============================================================================
%%% Common Tests Callbacks
%%% ============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% Init all groups
%% @end
%% -----------------------------------------------------------------------------
-spec all() -> lists:list().

all() ->
    [
        {group, elos}
    ].

%% -----------------------------------------------------------------------------
%% @doc
%% Groups
%% @end
%% -----------------------------------------------------------------------------
-spec groups() -> lists:list().

groups() ->
    [
        {elos, [sequence], [
            elos_echo,
            elos_log_files,
            elos_start_new_instance
        ]}
    ].

%% -----------------------------------------------------------------------------
%% @doc
%% Init per testcase
%% @end
%% -----------------------------------------------------------------------------
-spec init_per_testcase(Val :: any(), Config :: config()) -> Config :: config().

init_per_testcase(_, Config) ->
    Config.

%% -----------------------------------------------------------------------------
%% @doc
%% End per testcase
%% @end
%% -----------------------------------------------------------------------------
-spec end_per_testcase(config()) -> ok.

end_per_testcase(_Config) ->
    ok.

%% -----------------------------------------------------------------------------
%% @doc
%% Init per suite
%% @end
%% -----------------------------------------------------------------------------
-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
    ok = application:start(sasl),
    ok = application:start(elos),
    [application:set_env(elos, K, V) || {K, V} <- ?APP_CONFIG],
    Config.

%% -----------------------------------------------------------------------------
%% @doc
%% End per suite
%% @end
%% -----------------------------------------------------------------------------
-spec end_per_suite(config()) -> config().

end_per_suite(Config) ->
    ok = application:stop(sasl),
    ok = application:stop(elos),
    Config.


init_per_group(_, _Config) ->
    ok.

end_per_group(_, _Config) ->
    ok.

%%% ============================================================================
%%% Test cases for elos
%%% ============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% Check if log files was created
%% @end
%% -----------------------------------------------------------------------------
-spec elos_log_files(config()) -> ok.

elos_log_files(_Config) ->
    {ok, Files} = file:list_dir(log),
    Filename = "127.0.0.1:8181.log.1",
    case lists:member(Filename, Files) of
        true = Res->
            ct:pal("Filename ~p is exist in ~p~n", [Filename, Files]),
            ct:comment("Tests pass with test result = ~p", [Res]);
        false = Res ->
            ct:fail("EXPECTED ~p~nGOT: ~p~n", [true, Res])
    end.

%% -----------------------------------------------------------------------------
%% @doc
%% Send message to log and receive echo log level which was sent to confirm successful logging
%% Log level =:= 1 =:= alert
%% @end
%% -----------------------------------------------------------------------------
-spec elos_echo(config()) -> ok.

elos_echo(_Config) ->
    {ok, Socket} = gen_udp:open(8081, [{ip, {127, 127, 127, 127}}, {active, true}, binary, {reuseaddr, true}]),
    LogLevel = 1,
    LogMsg = <<"test message">>,
    Packet = <<LogLevel:8/integer, LogMsg/binary>>,
    ok = gen_udp:send(Socket, "127.0.0.1", 8181, Packet),
    {udp, Socket, {127, 0, 0, 1}, 8181, Res} = receive R -> R end,
    ok = gen_udp:close(Socket),
    case Res =:= <<LogLevel>> of
        true ->
            ct:pal("Sended ~p~nReceived ~p~n", [Packet, Res]),
            ct:comment("Tests pass with test result = ~p", [Res]);
        false ->
            ct:fail("EXPECTED ~p~nGOT: ~p~n", [<<1>>, Res])
    end.

%% -----------------------------------------------------------------------------
%% @doc
%% Start new instance and check if it's logged certain message level only
%% Log level =:= 3 =:= error
%% @end
%% -----------------------------------------------------------------------------
-spec elos_start_new_instance(config()) -> ok.

elos_start_new_instance(_Config) ->
    Config = #{
        name => '127.0.0.1:8282',
        ip => {127, 0, 0, 1},
        port => 8282,
        file => "log/127.0.0.1:8282.log",
        level => error,
        recbuf => 8192
    },
    {ok, _Pid} = elos:start_instance(Config),
    {ok, Socket} = gen_udp:open(8081, [{ip, {127, 127, 127, 127}}, {active, true}, binary, {reuseaddr, true}]),
    LogLevel = 3,
    SkipLogLevel = 4,
    LogMsg = <<"test message to new instance">>,
    SkipLogMsg = <<"test message to new instance should be skipped">>,
    Packet = <<LogLevel:8/integer, LogMsg/binary>>,
    SkipPacket = <<SkipLogLevel:8/integer, SkipLogMsg/binary>>,
    ok = gen_udp:send(Socket, "127.0.0.1", 8282, Packet),
    ok = gen_udp:send(Socket, "127.0.0.1", 8282, SkipPacket),
    {udp, Socket, {127, 0, 0, 1}, 8282, ResLogLevel} = receive R -> R end,
    ok = gen_udp:close(Socket),
    ok = elos:stop_instance('127.0.0.1:8282'),
    {error, closed} = gen_udp:send(Socket, "127.0.0.1", 8282, Packet),
    timer:sleep(100),
    case ResLogLevel =:= <<LogLevel>> of
        true ->
            {ok, Data} = file:read_file("log/127.0.0.1:8282.log.1"),
            case {binary:match(Data, <<"error: ", LogMsg/binary>>), binary:match(Data, <<"warning: ", SkipLogMsg/binary>>)} of
                {{_, _}, nomatch} ->
                    ct:pal("Sended ~p~nLog Data ~p~n", [Packet, Data]),
                    ct:comment("Log message = ~p was not found", [LogMsg]);
                Res ->
                    ct:fail("Tests failed with test result = ~p", [Res])
            end;
        false ->
            ct:fail("EXPECTED ~p~nGOT: ~p~n", [<<1>>, ResLogLevel])
    end.
