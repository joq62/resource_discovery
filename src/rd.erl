%% Author: uabjle
%% Created: 10 dec 2012
%% Description: TODO: Add description to application_org
-module(rd).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(SERVER,resource_discovery_server).
%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 add_target_resource_type/1,
	 add_local_resource/2,
	 delete_local_resource/2,	 
	 fetch_resources/1,
	 trade_resources/0,
	 rpc_call/4,
	 rpc_call/5,
	 rpc_multicall/4,
	 rpc_multicall/5,

	 get_state/0,
	 ping/0

	]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

add_target_resource_type(Type) ->
    gen_server:cast(?SERVER, {add_target_resource_type, Type}).

add_local_resource(Type, Resource) ->
    gen_server:cast(?SERVER, {add_local_resource, Type, Resource}).
delete_local_resource(Type, Resource) ->
    gen_server:call(?SERVER, {delete_local_resource, Type, Resource}).

fetch_resources(Type) ->
    gen_server:call(?SERVER, {fetch_resources, Type}).

trade_resources() ->
    gen_server:cast(?SERVER,{trade_resources}).

rpc_call(Type, Module, Function, Args)->
    ?SERVER:rpc_call(Type, Module, Function, Args).

rpc_call(Type, Module, Function, Args, Timeout) ->
    ?SERVER:rpc_call(Type, Module, Function, Args, Timeout).

rpc_multicall(Type, Module, Function, Args) ->
    ?SERVER:rpc_multicall(Type, Module, Function, Args).

rpc_multicall(Type, Module, Function, Args, Timeout) ->
     ?SERVER:rpc_multicall(Type, Module, Function, Args,Timeout).

ping() ->
    gen_server:call(?SERVER, {ping},infinity).
get_state() ->
    gen_server:call(?SERVER, {get_state},infinity).
%% ====================================================================!
%% External functions
%% ====================================================================!


%% ====================================================================
%% Internal functions
%% ====================================================================
