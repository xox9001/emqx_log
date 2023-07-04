#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

%%====================================================================
%% An usage test as simple and direct as possible. Enjoy.
%%====================================================================

main(_) ->
  io:format("~n-------------------------------------------~n"),
  io:format("Usage test :: Take a look at emqx_onlineLog please."),
  io:format("~n-------------------------------------------~n"),
  
  test_simple(),
  test_advanced().

test_simple() ->
  syslog:start_link(),
  
  syslog:send("(simple) info message"),

  ok.
  
test_advanced() ->
  syslog:start_link(my_syslog, "/tmp/emqx/"),
  syslog:send(my_syslog, "(advanced - send) debug message"),
  
  ok.
