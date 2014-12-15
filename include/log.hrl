-export ([testlog/0]).

'_debug' (Msg)                                           -> '_debug' (Msg, []).
'_debug' (Msg, Args)                                     -> send_to_log (Msg, Args, debug).
'_debug' (Condition, _Msg, _Args) when not Condition     -> ok;
'_debug' (Condition, Msg, Args) when Condition           -> '_debug'(Msg, Args).
'_debug' (Condition, Msg, Args, _Else) when Condition    -> '_debug'(Msg, Args);
'_debug' (Condition, Msg, Args, Else) when not Condition -> send_to_log (Msg, Args, Else).

'_trace' (Msg)                                           -> '_trace' (Msg, []).
'_trace' (Msg, Args)                                     -> send_to_log (Msg, Args, trace).
'_trace' (Condition, _Msg, _Args) when not Condition     -> ok;
'_trace' (Condition, Msg, Args) when Condition           -> '_trace'(Msg, Args).
'_trace' (Condition, Msg, Args, _Else) when Condition    -> '_trace'(Msg, Args);
'_trace' (Condition, Msg, Args, Else) when not Condition -> send_to_log (Msg, Args, Else).

'_info' (Msg)                                            -> '_info' (Msg, []).
'_info' (Msg, Args)                                      -> send_to_log (Msg, Args, info).
'_info' (Condition, _Msg, _Args) when not Condition      -> ok;
'_info' (Condition, Msg, Args) when Condition            -> '_info'(Msg, Args).
'_info' (Condition, Msg, Args, _Else) when Condition     -> '_info'(Msg, Args);
'_info' (Condition, Msg, Args, Else) when not Condition  -> send_to_log (Msg, Args, Else).

'_notice' (Msg)                                          -> '_notice' (Msg, []).
'_notice' (Msg, Args)                                    -> send_to_log (Msg, Args, notice).
'_notice' (Condition, _Msg, _Args) when not Condition    -> ok;
'_notice' (Condition, Msg, Args) when Condition          -> '_notice'(Msg, Args).
'_notice' (Condition, Msg, Args, _Else) when Condition   -> '_notice'(Msg, Args);
'_notice' (Condition, Msg, Args, Else) when not Condition-> send_to_log (Msg, Args, Else).

'_warning' (Msg)                                         -> '_warning' (Msg, []).
'_warning' (Msg, Args)                                   -> send_to_log (Msg, Args, warning).
'_warning' (Condition, _Msg, _Args) when not Condition   -> ok;
'_warning' (Condition, Msg, Args) when Condition         -> '_warning'(Msg, Args).
'_warning' (Condition, Msg, Args, _Else) when Condition  -> '_warning'(Msg, Args);
'_warning' (Condition, Msg, Args, Else) when not Condition->send_to_log (Msg, Args, Else).

'_err' (Msg)                                             -> '_err' (Msg, []).
'_err' (Msg, Args)                                       -> send_to_log (Msg, Args, err).
'_err' (Condition, _Msg, _Args) when not Condition       -> ok;
'_err' (Condition, Msg, Args) when Condition             -> '_err'(Msg, Args).
'_err' (Condition, Msg, Args, _Else) when Condition      -> '_err'(Msg, Args);
'_err' (Condition, Msg, Args, Else) when not Condition   -> send_to_log (Msg, Args, Else).

'_crit' (Msg)                                            -> '_crit' (Msg, []).
'_crit' (Msg, Args)                                      -> send_to_log (Msg, Args, crit).
'_crit' (Condition, _Msg, _Args) when not Condition      -> ok;
'_crit' (Condition, Msg, Args) when Condition            -> '_crit'(Msg, Args).
'_crit' (Condition, Msg, Args, _Else) when Condition     -> '_crit'(Msg, Args);
'_crit' (Condition, Msg, Args, Else) when not Condition  -> send_to_log (Msg, Args, Else).

'_alert' (Msg)                                           -> '_alert' (Msg, []).
'_alert' (Msg, Args)                                     -> send_to_log (Msg, Args, alert).
'_alert' (Condition, _Msg, _Args) when not Condition     -> ok;
'_alert' (Condition, Msg, Args) when Condition           -> '_alert'(Msg, Args).
'_alert' (Condition, Msg, Args, _Else) when Condition    -> '_alert'(Msg, Args);
'_alert' (Condition, Msg, Args, Else) when not Condition -> send_to_log (Msg, Args, Else).

'_emerg' (Msg)                                           -> '_emerg' (Msg, []).
'_emerg' (Msg, Args)                                     -> send_to_log (Msg, Args, emerg).
'_emerg' (Condition, _Msg, _Args) when not Condition     -> ok;
'_emerg' (Condition, Msg, Args) when Condition           -> '_emerg'(Msg, Args).
'_emerg' (Condition, Msg, Args, _Else) when Condition    -> '_emerg'(Msg, Args);
'_emerg' (Condition, Msg, Args, Else) when not Condition -> send_to_log (Msg, Args, Else).

send_to_log (Msg, Args, MsgLvl) ->
  {ok, Lvl} = logger:current_level(self(), ?MODULE, <<?MODULE_STRING>>, node()),
  send_to_log(Msg, Args, logger:level_to_integer(MsgLvl), logger:level_to_integer(Lvl)).

send_to_log(_Msg, _Args, MsgLvl, CurLvl) when MsgLvl > CurLvl ->
  ok;
send_to_log(Msg, Args, MsgLvl, _CurLvl) ->
  MsgBin = format_msg(Msg, Args, MsgLvl),
  logger:message(MsgBin).

testlog () ->
  '_trace' ("test message"),
  '_trace' ("test message with param ~w", [1]),
  '_trace' (false, "", []),
  '_trace' (true, "", [], trace),
  '_debug' ("test message"),
  '_debug' ("test message with param ~w", [1]),
  '_debug' (false, "", []),
  '_debug' (false, "", [], trace),
  '_info' ("test message"),
  '_info' ("test message with param ~w", [1]),
  '_info' (false, "", []),
  '_info' (false, "", [], trace),
  '_notice' ("test message"),
  '_notice' ("test message with param ~w", [1]),
  '_notice' (false, "", []),
  '_notice' (false, "", [], trace),
  '_warning' ("test message"),
  '_warning' ("test message with param ~w", [1]),
  '_warning' (false, "", []),
  '_warning' (false, "", [], trace),
  '_err' ("test message"),
  '_err' ("test message with param ~w", [1]),
  '_err' (false, "", []),
  '_err' (false, "", [], trace),
  '_crit' ("test message"),
  '_crit' ("test message with param ~w", [1]),
  '_crit' (false, "", []),
  '_crit' (false, "", [], trace),
  '_alert' ("test message"),
  '_alert' ("test message with param ~w", [1]),
  '_alert' (true, "test message with param ~w", [1]),
  '_alert'(false, "", [], trace),
  '_emerg' ("test message"),
  '_emerg' ("test message with param ~w", [1]),
  '_emerg' (true, "test message with param ~w", [1]),
  '_emerg'(false, "", [], trace),
  '_warning'("unicode test/тест русского"),
  '_warning'("unicode test: ~s", [<<"тест русского"/utf8>>]),
  ok.

format_msg(Msg, Args, MsgLvl) ->
  {_, _, S1} = Now = os:timestamp(),
  {{Y, M, D}, {H, Mi, S}} = calendar:now_to_local_time(Now),
  Node = node(),
  Module = ?MODULE,
  Pid = self(),
  try
    list_to_binary(
      io_lib:format (
        "~4.4.0w/~2.2.0w/~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w.~6.6.0w ~s ~w ~s ~s: " ++ hr_msg(Msg) ++ "~n",
        [Y, M, D, H, Mi, S, S1, Node, Pid, Module, logger:level_to_atom(MsgLvl)] ++ hr(Args)))
  catch
    _:E ->
      S = erlang:get_stacktrace(),
      error_logger:error_msg("creating msg: '~65535p'~n"
                             "with args ~65535p~n"
                             "Exception: ~65535p~n"
                             "Stacktrace: ~65535p~n",
                             [Msg, Args, E, S]),
      <<>>
  end.

hr(<<>>) ->
  "<<>>";
hr(<<Bin:255/binary, Rest/binary>> = Full) when Rest =/= <<>> ->
  lists:flatten(io_lib:format("~w (255 of ~w bytes)", [Bin, byte_size(Full)]));
hr(<<Bin:255/binary, Rest/binary>> = Full) when Rest =/= <<>> ->
  lists:flatten(
    io_lib:format("<<16#~ts:~w ...>> (255 of ~w bytes)",
                  [hr_binary(Bin), bit_size(Bin), byte_size(Full)]));
hr(Bin) when is_binary(Bin) ->
  lists:flatten(io_lib:format("~w", [Bin]));
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
  Bin.
%  typextfun:to_hex(Bin).

hr_msg(M) -> hr_msg([], M).

hr_msg(Result, [$~, A | Tail]) when A =:= $p; A =:= $w ->
  hr_msg([$p, $5, $3, $5, $5, $6, $~ | Result], Tail);
hr_msg(Result, [S | Tail]) ->
  hr_msg([S | Result], Tail);
hr_msg(Result, []) ->
  lists:reverse(Result).
