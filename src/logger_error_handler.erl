%%%-------------------------------------------------------------------
%%% @author Ilya Ashchepkov
%%% @copyright 2014 NskAvd
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(logger_error_handler).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("log.hrl").
%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({error_report, GLeader, {_From, crash_report, [OwnReport, LinkReport]}}, State) ->
  AppName = application:get_application(GLeader),
  crash_report(AppName, OwnReport, LinkReport),
  {ok, State};
handle_event({error_report, _GLeader, {_From, supervisor_report, Report}}, State) ->
  Sup = proplists:get_value(supervisor, Report),
  Context = proplists:get_value(errorContext, Report),
  Reason = proplists:get_value(reason, Report),
  Offender = proplists:get_value(offender, Report),
  supervisor_report(Context, Sup, Offender, Reason),
  {ok, State};
handle_event({info_report, _GLeader, {_From, progress, Report}}, State) ->
  progress(Report),
  {ok, State};
handle_event({info_report, _GLeader,
              {_From, std_info, [{application, AppName}, {exited, Reason} | _]}}, State) ->
  application_stop(AppName, Reason),
  {ok, State};
handle_event(Event, State) ->
  trace("event ~w", [Event]),
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
  debug("call ~w", [_Request]),
  Reply = ok,
  {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  trace("info ~w", [_Info]),
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
supervisor_report(child_terminated, Sup, Offender, Reason) ->
  warning("~w supervised by ~w abnormal termination: ~w", [Offender, Sup, Reason]);
supervisor_report(Context, _, _, _) ->
  warning("unhandled supervisor report context ~w", [Context]).

crash_report({ok, AppName}, OwnReport, LinkReport) ->
  crash_report(AppName, OwnReport, LinkReport);
crash_report(AppName, OwnReport, []) ->
  {InitModule, InitFun, InitArgs} = proplists:get_value(initial_call, OwnReport),
  Pid = proplists:get_value(pid, OwnReport),
  Name = proplists:get_value(registered_name, OwnReport),
  PIdent = proc_lib_name(Pid, Name),
  Stacktrace = proplists:get_value(error_info, OwnReport),
  emerg("application ~w failed start ~w (~w:~w/~w), stacktrace ~w",
        [AppName, PIdent, InitModule, InitFun, length(InitArgs), Stacktrace]);
crash_report(AppName, OwnReport, LinkReport) ->
  crash_report(AppName, OwnReport, []),
  warning("unhandled link report ~w", [LinkReport]).

proc_lib_name(Pid, Name)
  when
    Name =:= undefined;
    Name =:= []
    ->
  Pid;
proc_lib_name(_Pid, Name) ->
  Name.

progress([{application, AppName}, {started_at, Node}])
  when Node =:= node() ->
  notice("application ~w started", [AppName]);
progress([{application, AppName}, {started_at, Node}]) ->
  notice("application ~w started at node ~w", [AppName, Node]);
progress(Report) ->
  debug("progress: ~w", [Report]).

application_stop(AppName, Reason)
  when
    Reason =:= stopped
    ->
  notice("application ~w stopped", [AppName]);
application_stop(AppName, Reason) ->
  warning("application ~w stopped with reason ~w", [AppName, Reason]).

%% vim: ft=erlang
