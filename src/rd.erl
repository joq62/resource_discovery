%% Author: uabjle
%% Created: 10 dec 2012
%% Description: TODO: Add description to application_org
-module(rd).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/2,
	 stop/1
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 add_target_resource_type/1,
	 add_local_resource/2,
	 fetch_resources/1,
	 trade_resources/0
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

add_target_resource_type(Type)->
    rd_server:add_target_resource_type(Type).
add_local_resource(Type,Instance)->
    rd_server:add_local_resource(Type,Instance).
	 
fetch_resources(Type)->
    rd_server:fetch_resources(Type).

trade_resources()->
    rd_server:trade_resources().

%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _StartArgs) ->
    ok=ensure_contact(),
    {ok,Pid}= rd_sup:start_link(),
    {ok,Pid}.
   
%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
ensure_contact()->
    AppFile=filename:join("ebin",atom_to_list(?MODULE)++".app"),
    {ok,[{application,?MODULE,Info}]}=file:consult(AppFile),
    {env,EnvList}=lists:keyfind(env,1,Info),
    {connect_nodes,ConnectNodes}=lists:keyfind(connect_nodes,1,EnvList),
    ok=case ConnectNodes of
	   []->
	       {error,[no_connect_nodes_defined,[],?FUNCTION_NAME,?MODULE,?LINE]};
	   _->
	       NumTries=10,
	       Interval=2000, % 2 seconds
	       case connect(ConnectNodes,NumTries,Interval,false) of
		   false->
		       {error,[no_connect_nodes_available,ConnectNodes,?FUNCTION_NAME,?MODULE,?LINE]};
		   true->
		       ok
	       end
       end,
    ok.
	       
		       
connect(_,_,_,true) ->
    true;
connect(_,0,_,Result)->
    Result;

connect(ConnectNodes,N,I,_)->
    Answering=[Node||Node<-ConnectNodes,net_adm:ping(Node)=:=pong],
    case Answering of
	[]->
	    timer:sleep(I),
	    NewN=N-1,
	    Result=false;
	_ ->
	    NewN=N-1,
	    Result=true
    end,
    connect(ConnectNodes,NewN,I,Result).

