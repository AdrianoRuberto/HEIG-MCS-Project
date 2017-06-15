%%%-------------------------------------------------------------------
%% @doc sentibot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sentibot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  EchoModule = sentibot_serv,
  EchoChild = #{id => EchoModule,
    start => {EchoModule, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker},
  ChildSpecs = [EchoChild],
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
