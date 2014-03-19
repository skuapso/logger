-export ([testlog/0]).

trace (Msg) ->
  trace (Msg, []).
trace (Msg, Args) ->
  send_to_log (Msg, Args, trace).

debug (Msg) ->
  debug (Msg, []).
debug (Msg, Args) ->
  send_to_log (Msg, Args, debug).

info (Msg) ->
  info (Msg, []).
info (Msg, Args) ->
  send_to_log (Msg, Args, info).

notice (Msg) ->
  notice (Msg, []).
notice (Msg, Args) ->
  send_to_log (Msg, Args, notice).

warning (Msg) ->
  warning (Msg, []).
warning (Msg, Args) ->
  send_to_log (Msg, Args, warning).

err (Msg) ->
  err (Msg, []).
err (Msg, Args) ->
  send_to_log (Msg, Args, err).

crit(Msg) ->
  crit(Msg, []).
crit(Msg, Args) ->
  send_to_log(Msg, Args, crit).

alert(Msg) ->
  alert(Msg, []).
alert(Msg, Args) ->
  send_to_log(Msg, Args, alert).

emerg(Msg) ->
  emerg(Msg, []).
emerg(Msg, Args) ->
  send_to_log(Msg, Args, emerg).

send_to_log (Msg, Args, MsgLvl) ->
  {ok, Lvl} = logger:current_level(self(), ?MODULE, <<?MODULE_STRING>>, node()),
  send_to_log(Msg, Args, logger:level_to_integer(MsgLvl), logger:level_to_integer(Lvl)).

send_to_log(_Msg, _Args, MsgLvl, CurLvl) when MsgLvl > CurLvl ->
  ok;
send_to_log(Msg, Args, MsgLvl, _CurLvl) ->
  MsgBin = format_msg(Msg, Args, MsgLvl),
  logger:message(MsgBin).

testlog () ->
  trace ("test message"),
  trace ("test message with param ~w", [1]),
  debug ("test message"),
  debug ("test message with param ~w", [1]),
  info ("test message"),
  info ("test message with param ~w", [1]),
  notice ("test message"),
  notice ("test message with param ~w", [1]),
  warning ("test message"),
  warning ("test message with param ~w", [1]),
  err ("test message"),
  err ("test message with param ~w", [1]),
  crit ("test message"),
  crit ("test message with param ~w", [1]),
  alert ("test message"),
  alert ("test message with param ~w", [1]),
  emerg ("test message"),
  emerg ("test message with param ~w", [1]),
  warning("unicode test/тест русского"),
  warning("unicode test: ~s", [<<"тест русского"/utf8>>]),
  ok.

format_msg(Msg, Args, MsgLvl) ->
  {{Y, M, D}, {H, Mi, S}} = erlang:localtime(),
  Node = node(),
  Module = ?MODULE,
  Pid = self(),
  try
    list_to_binary(
      io_lib:format (
        "~4.4.0w/~2.2.0w/~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w ~s ~w ~s ~s: " ++ hr_msg(Msg) ++ "~n",
        [Y, M, D, H, Mi, S, Node, Pid, Module, logger:level_to_atom(MsgLvl)] ++ hr(Args)))
  catch
    _:_ ->
      err("error while creating msg '~w' with args ~w", [Msg, Args]),
      <<>>
  end.

hr(<<>>) ->
  "<<>>";
hr(<<Bin:2048/binary, Rest/binary>> = Full) when Rest =/= <<>> ->
  lists:flatten(
    io_lib:format("<<16#~ts:~w ...>> (~w bytes)",
                  [hr_binary(Bin), bit_size(Bin), byte_size(Full)]));
hr(Bin) when is_binary(Bin) ->
  lists:flatten(
    io_lib:format("<<16#~ts:~w>> (~w bytes)",
                  [hr_binary(Bin), bit_size(Bin), byte_size(Bin)]));
hr(T) when is_tuple(T) ->
  list_to_tuple(hr(tuple_to_list(T)));
hr(L) when is_list(L) ->
  lists:map(
    fun(X) ->
        hr(X)
    end, L);
hr(X) -> X.

hr_binary(Bin) ->
  typextfun:to_hex(Bin).

hr_msg(M) -> hr_msg([], M).

hr_msg(Result, [$~, A | Tail]) when A =:= $p; A =:= $w ->
  hr_msg([$p, $5, $3, $5, $5, $6, $~ | Result], Tail);
hr_msg(Result, [S | Tail]) ->
  hr_msg([S | Result], Tail);
hr_msg(Result, []) ->
  lists:reverse(Result).
