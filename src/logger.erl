-module(logger).

-behaviour(application).
-behaviour(supervisor).

%% API
-export([
  level_to_integer/1,
  level_to_atom/1,
  current_level/4,
  message/1,
  set/2
  ]).

%% Application API
-export([
  start/0,
  start/2,
  stop/1
  ]).

%% Supervisor API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include("log.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
level_to_integer(debug)   -> 8;
level_to_integer(trace)   -> 7;
level_to_integer(info)    -> 6;
level_to_integer(notice)  -> 5;
level_to_integer(warning) -> 4;
level_to_integer(err)     -> 3;
level_to_integer(crit)    -> 2;
level_to_integer(alert)   -> 1;
level_to_integer(emerg)   -> 0;
level_to_integer(L) when is_integer(L) -> L.

level_to_atom(8) -> debug;
level_to_atom(7) -> trace;
level_to_atom(6) -> info;
level_to_atom(5) -> notice;
level_to_atom(4) -> warning;
level_to_atom(3) -> err;
level_to_atom(2) -> crit;
level_to_atom(1) -> alert;
level_to_atom(0) -> emerg;
level_to_atom(L) when is_atom(L) -> L.

current_level(_Pid, Module, ModuleBin, _Node) ->
  Lvl = case ets:match(?MODULE, {Module, '$1'}) of
    [] ->
      case ets:select(?MODULE, [{{'$1', '$2'}, [{is_binary,'$1'}], [{{'$1', '$2'}}]}]) of
        [] ->
          default();
        L ->
          L1 = lists:filtermap(
                 fun({X, Y}) when is_binary(X) ->
                      Len = byte_size(X),
                      case ModuleBin of
                        <<X:Len/binary, _/binary>> ->
                          {true, {-byte_size(X), Y}};
                        _ -> false
                      end
                  end, L),
          case lists:sort(L1) of
            [] -> default();
            [{_, PrefixLvl} | _] -> PrefixLvl
          end
      end;
    [[ModuleLvl]] -> ModuleLvl
  end,
  {ok, Lvl}.

message(Msg) ->
  gen_event:notify(logger_event, {message, Msg}).

set(default, Lvl) when is_atom(Lvl) ->
  ets:insert(?MODULE, {default, Lvl}),
  ok;
set(Module, default) when is_atom(Module) ->
  ets:delete(?MODULE, Module),
  ok;
set(Module, default) when is_list(Module) ->
  ets:delete(?MODULE, list_to_binary(Module)),
  ok;
set(Module, Lvl) when (is_atom(Module) and is_atom(Lvl)) ->
  case ets:match(?MODULE, {default, '$1'}) of
    [] -> ok;
    [[Lvl]] -> ets:delete(?MODULE, Module);
    [[_Default]] -> ets:insert(?MODULE, {Module, Lvl})
  end,
  ok;
set(Prefix, Lvl) when (is_list(Prefix) and is_atom(Lvl)) ->
  ets:insert(?MODULE, {list_to_binary(Prefix), Lvl}),
  ok.
%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {'_err'or, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
  application:start(?MODULE).

start(_Type, Args) ->
  start_link(Args).

start_link(Args) ->
  Reply = supervisor:start_link({local, ?MODULE}, ?MODULE, Args),
  Handlers = misc:get_env(?MODULE, engine, Args),
  lists:map(fun({Handler, Opts}) ->
        erlang:apply(Handler, add_handler, Opts)
    end, Handlers),
  Reply.

stop(_State) ->
  ok.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {'_err'or, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
  error_logger:add_report_handler(logger_error_handler),
  ets:new(?MODULE, [set, public, named_table]),
  Default = misc:get_env(?MODULE, default, Args),
  set(default, Default),
  ModuleLevel = misc:get_env(?MODULE, module, Args),
  lists:map(fun({Module, Lvl}) ->
        set(Module, Lvl)
    end, ModuleLevel),
  {
    ok, {
      {one_for_one, 5, 10},
      [
        {
          ?MODULE,
          {gen_event, start_link, [{local, logger_event}]},
          permanent,
          5000,
          worker,
          []
        }
      ]
    }
  }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
default() ->
  [[Default]] = ets:match(?MODULE, {default, '$1'}),
  Default.
