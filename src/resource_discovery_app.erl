%%%-------------------------------------------------------------------
%% @doc org public API
%% @end
%%%-------------------------------------------------------------------
 
-module(resource_discovery_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    resource_discovery_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
