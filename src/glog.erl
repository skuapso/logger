-module (glog).

-behaviour (gen_event).

-export ([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-include_lib("logger/include/log.hrl").

init (EvRef) ->
  {ok, EvRef}.

handle_event ({_Node, ?MODULE, _Pid, _Msg, _Args, _MsgLvl}, EvRef) ->
  {ok, EvRef};
handle_event (Msg, EvRef) ->
  try
    gen_event:notify (EvRef, Msg)
  catch
    _:_ ->
      warning ("can't send to ~w", [EvRef]),
      ok
  end,
  {ok, EvRef}.

handle_call (Msg, EvRef) ->
  try
    R = gen_event:call (EvRef, Msg),
    {ok, R, EvRef}
  catch
    _:_ ->
      {ok, ok, EvRef}
  end.

handle_info (_Msg, EvRef) ->
  {ok, EvRef}.

terminate (_Reason, _S) ->
  ok.

code_change (_OldVsn, EvRef, _Extra) ->
  {ok, EvRef}.
