-module(emqx_onlineLog).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start_link/0, start_link/2
         , send/1, send/2, send/3
		,rotate/0
		,closeFd/1
		,return_fill/1
		]).

-rest_api(#{ name => rotateLog
           , method => 'GET'
           , path => "/onlineLog_rotate"
           , func => rotate
           , descr => "rotateLog"
           }).

-record(state, {socket, log_path}).

%% -define(DEFAULT_FACILITY, local0).

%%====================================================================
%% api callbacks
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ["/tmp/emqx/"], []).

start_link(Name,LogPath) when is_atom(Name) ->
    gen_server:start_link({local, Name},  ?MODULE, [LogPath], []).

send(Msg) ->
    gen_server:call(?MODULE,{send,Msg}).

send(Name, Msg) when is_list(Msg) ->
    send(Name, Msg, []).

send(Name, Msg, Opts) when is_list(Msg), is_list(Opts) ->
%% sync
%%     gen_server:call(Name, {send, Msg}).
%% async
	gen_server:cast(Name, {send, Msg}).

rotate()->
	gen_server:call(?MODULE, {rotate},20000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([LogPath]) ->
	Path = LogPath++"log",
	case file:open(Path, [ write, {delayed_write,104857600,3000}, binary]) of
		{ok, FD} ->
			{ok, #state{socket = FD,log_path=Path}};
		{error, Reason} ->
            {stop, Reason}
	end.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({send,Msg},_From, #state{socket=Socket}=State) ->
	Packet = list_to_binary([Msg]),
	file:write(Socket, Packet),
    {reply, ok,State};

handle_call({rotate},_From, #state{socket=OFD,log_path=Path}) ->
	{{Year, Month, Day}, {Hour, Minite, Second}} = calendar:local_time(),
	NewLogPath = io_lib:format("~s-~w~s~s~s~s~s", [Path,Year,return_fill(Month),return_fill(Day),return_fill(Hour),return_fill(Minite),return_fill(Second)]),
	case file:open(NewLogPath, [ write, {delayed_write,104857600,3000}, binary]) of
		{ok, FD} ->
			spawn(emqx_onlineLog,closeFd,[OFD]),
			io:format("LogRotateSuccess:~w,~w,~s~n",[FD,OFD,NewLogPath]),
		%%	io:format(erlang:is_process_alive(Pid)),
		%%	timer:sleep(10000),
		%%	io:format(erlang:is_process_alive(Pid)),
			{reply,ok, #state{socket = FD,log_path=Path}};
		{error, Reason} ->
			io:format("open fail:~p~n",[Reason]),
            {reply,stop, Reason}
	end.

return_fill(T)->
	if 
		T < 10 -> io_lib:format("0~w", [T]);
		true -> io_lib:format("~w", [T])
	end.
		
%%关闭文件
closeFd(FD) ->
%%	io:format("closeFd:~w~n",[calendar:local_time()]),
	timer:sleep(10000),
	io:format("closeFd_At:~w~n",[calendar:local_time()]),
%%	io:format("closeFd:~w~n",[calendar:local_time()]),
%%	io:format("closeFd:~w~n",[FD]),
	Packet = list_to_binary(["[Close]Rotate_Log\n"]),
	file:write(FD, Packet),
	file:sync(FD),
	file:close(FD).

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({send, Msg},#state{socket=Socket}=State) ->
	Packet = list_to_binary([Msg]),
	file:write(Socket, Packet),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% format_timestamp(TS) ->
%%     {{Y, M, D}, {H, MM, S}} = calendar:now_to_universal_time(TS),
%%     US = element(3, TS),
%%     io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ",
%%                   [Y,M,D, H,MM,S,US]).

