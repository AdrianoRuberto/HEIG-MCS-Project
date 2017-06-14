%%%-------------------------------------------------------------------
%%% @author Adriano
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. juin 2017 15:45
%%%-------------------------------------------------------------------
-module(sentibot_serv).
-author("Adriano").

-behaviour(gen_server).

%% API
-export([start_link/0, slack_connect/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  slack_connect(),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_url(ConnPid, Dest) ->
  StreamRef = gun:get(ConnPid, Dest),
  receive
    {gun_response, ConnPid, StreamRef, nofin, Status, Headers} ->
      receive_data(ConnPid, StreamRef)
  after 1000 ->
    exit(timeout)
  end.

receive_data(ConnPid, StreamRef) ->
  receive
    {gun_data, ConnPid, StreamRef, nofin, Data} ->
      receive_data(ConnPid, StreamRef);
    {gun_data, ConnPid, StreamRef, fin, Data} ->
      maps:get(<<"url">>, jsone:decode(Data))
  after 1000 ->
    exit(timeout)
  end.

dump_mailbox() ->
  receive
    A ->
      erlang:display(A),
      dump_mailbox()
  after 5000 ->
    ok
  end.

handle_frame(SocketPid) ->
  receive
    {gun_ws, SocketPid, {text, Data}} ->
      Json = jsone:decode(Data),
      case maps:get(<<"type">>, Json) of
        <<"message">> -> handle_message(Json);
        _ -> ignore
      end;
    _ -> ignore
  end,
  handle_frame(SocketPid).

handle_message(Message) ->
  Text = binary_to_list(maps:get(<<"text">>, Message)),
  case re:run(Text, "I\s+am\s+((?:(?:not|very)\s+)*[^\s]+)") of
    {match, [_, {Offset, Length}]} ->
      Emotion = string:substr(Text, Offset + 1, Length),
      erlang:display(Emotion);
    _ -> erlang:display("nop")
  end.


slack_connect() ->
  {ok, ConnPid} = gun:open("slack.com", 443),
  Url = get_url(ConnPid, "/api/rtm.connect?token=xoxb-195222969522-5QD8TIlAupmPLkN2FOAgDBiU"),
  [_, _, Host, _, IdKey] = re:split(Url, "/"),
  {ok, SocketPid} = gun:open(binary_to_list(Host), 443),
  Upgrade = gun:ws_upgrade(SocketPid, "/websocket/" ++ binary_to_list(IdKey)),

  receive
    {gun_ws_upgrade, _, ok, _} ->
      ok
%% More clauses here as needed.
  after 1000 ->
    exit(timeout)
  end,

  handle_frame(SocketPid).


%erlang:display(Foo).

%gun:ws_upgrade(ConnPid, "/websocket"),
