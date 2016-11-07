%% @author ethrbh
%% @doc @todo Add description to m1.


-module(m1).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, stop/0]).
-export([ping/0, ping/2]).
-export([get_version/0]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {
				version="0.0.1"}).

%% ====================================================================
%% Includes
%% ====================================================================
-include("common.hrl").

%% ====================================================================
%% Defines
%% ====================================================================
-define(SERVER, ?MODULE).

%% ====================================================================
%% Start IGS main process. This process is the responsible for
%% - init MNESIA tables
%% - init ETS tables
%% ====================================================================
start_link()->
	case catch erlang:whereis(?SERVER) of
		P when is_pid(P) ->
			%% Process is alive, must kill first
			stop(),
			
			%% Start the server
			gen_server:start_link({local, ?SERVER}, ?MODULE, [], []);
		
		_-> %% Process is not alive, let's start it.
			gen_server:start_link({local, ?SERVER}, ?MODULE, [], [])
	end.

%% ====================================================================
%% Stop IGS main process
%% ====================================================================
stop()->
	case catch erlang:whereis(?SERVER) of
		P when is_pid(P) ->
			%% Process is alive, must kill first
			gen_server:call(P, stop, 3000);
			
		_-> %% Process is not alive.
			ok
	end.

%% ====================================================================
%% Ping itself
-spec ping() -> {ok, pong} | {error, term()}.
%% ====================================================================
ping() ->
	case catch erlang:whereis(?SERVER) of
		P when is_pid(P) ->
			gen_server:call(P, ping, 3000);
			
		_-> %% Process is not alive.
			{error, {?SERVER, "does not alive"}}
	end.

%% ====================================================================
%% Ping an other process
-spec ping(Mod :: atom(), Proc :: atom() | pid()) -> {ok, pong} | {error, term()}.
%% ====================================================================
ping(Mod, Proc) ->
	case catch erlang:whereis(?SERVER) of
		P when is_pid(P) ->
			gen_server:call(P, {ping, Mod, Proc}, 3000);
			
		_-> %% Process is not alive.
			{error, {?SERVER, "does not alive"}}
	end.

%% ====================================================================
%% Give the version of server
-spec get_version() -> {ok, string()} | {error, term()}.
%% ====================================================================
get_version() ->
	case catch erlang:whereis(?SERVER) of
		P when is_pid(P) ->
			gen_server:call(P, get_version, 3000);
			
		_-> %% Process is not alive.
			{error, {?SERVER, "does not alive"}}
	end.

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(stop, _From, State) ->
	?DO_INFO("!!!! Erlang Upg Test is going to stop !!!!.", []),
    {stop, normal, ok, State};

handle_call(ping, _From, State) ->
	{reply, {ok, pong}, State};

handle_call({ping, Mod, Proc}, _From, State) ->
	case catch erlang:apply(Mod, ping, [Proc]) of
		ok ->
			{reply, {ok, pong}, State};
		Err ->
			{reply, {error, Err}, State}
	end;

handle_call(get_version, _From, State) ->
	{reply, {ok, State#state.version}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(Msg, State) ->
	?DO_INFO("handle_cast", 
			 [{msg, Msg},
			  {state, State}]),
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
	?DO_INFO("handle_info", 
			 [
			 {info, Info},
			 {state, State}]),
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
	?DO_INFO("terminate", 
			 [
			 {reason, Reason},
			 {state, State}]),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change({down, Vsn}, State, _Extra) ->
	?DO_INFO("Downgrade", 
			 [
			   {module, ?MODULE},
			   {fromVer, State#state.version},
			   {toVer, Vsn},
			   {extra, _Extra}
			   ]),
    {ok, State#state{version = Vsn}};
code_change(Vsn, State, _Extra) ->
	?DO_INFO("Upgrade", 
			 [
			   {module, ?MODULE},
			   {fromVer, State#state.version},
			   {toVer, Vsn},
			   {extra, _Extra}
			   ]),
   {ok, State#state{version = Vsn}}.
%% code_change(OldVsn, State, Extra) ->
%% 	?DO_INFO("code_change", 
%% 			 [
%% 			 {oldVsn, OldVsn},
%% 			 {state, State},
%% 			 {extra, Extra}]),
%%     {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


