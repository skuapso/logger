-module (flog).

-behaviour (gen_event).

-export([add_handler/0, add_handler/1, add_handler/2]).

-export ([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-include_lib("logger/include/log.hrl").

add_handler() ->
  add_handler([]).

add_handler(File) ->
  add_handler(File, []).

add_handler(File, Options) ->
  gen_event:add_handler(logger_event, ?MODULE, {File, Options}).

init({file_descriptor, prim_file, _} = Fd) ->
  Msg = format_msg("reinstalled", [], logger:level_to_integer(notify)),
  file:write(Fd, Msg),
  {ok, Fd};
init({[], Options}) ->
  init({atom_to_list(node()), Options});
init({File, []}) ->
  init({File, [append]});
init({File, Options}) ->
  {ok, Fd} = file:open(File, [raw | Options]),
  {ok, Fd}.

handle_event({message, Msg}, Fd) when is_binary(Msg) ->
  file:write(Fd, Msg),
  {ok, Fd};
handle_event (Msg, Fd) ->
  err ("unhandled msg: ~w", [Msg]),
  {ok, Fd}.

handle_call (Req, Fd) ->
  err ("unhandled call: ~w", [Req]),
  {ok, unknown, Fd}.

handle_info (Msg, Fd) ->
  err ("unhandled info: ~w", [Msg]),
  {ok, Fd}.

terminate(stop, Fd) ->
  file:close(Fd);
terminate (Reason, Fd) ->
  Msg = format_msg("terminating with reason ~w", [Reason], emerg),
  file:write(Fd, Msg),
  spawn(?MODULE, add_handler, [Fd]),
  ok.

code_change (_OldVsn, S, _Extra) ->
  {ok, S}.
