%%%-------------------------------------------------------------------
%% @doc sentibot public API
%% @end
%%%-------------------------------------------------------------------

-module(sentibot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  sentibot_sup:start_link().


%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
