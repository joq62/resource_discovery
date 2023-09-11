%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(single_tests).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(ClusterName,"test_cluster").



%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(),
    ok=normal_tests(),
    ok=rpc_tests(),
    ok=rpc_multi_tests(),
    ok=stop_start_tests(),
   

    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
 
    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
rpc_multi_tests()->

    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]), 
    
      io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
rpc_tests()->

    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]), 
  

    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
stop_start_tests()->

    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]), 
 
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
normal_tests()->

    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]), 
    Node0=node(),
    Local0=[{type0,Node0},{type1,Node0}],
    Target0=[type1,type2],
    % Add Node0
    pong=rpc:call(Node0,rd,ping,[]),
    rpc:call(Node0,rd,get_state,[]),

    [rpc:call(Node0,rd,add_local_resource,[Type,Instance],5000)||{Type,Instance}<-Local0],
    [rpc:call(Node0,rd,add_target_resource_type,[Type],5000)||Type<-Target0],
    ok=rpc:call(Node0,rd,trade_resources,[],5000),

     
    timer:sleep(3000),
    {ok,[Node0]}=rpc:call(Node0,rd,fetch_resources,[type1],5000),
    
    kuk=rpc:call(Node0,rd,get_state,[]),
    

    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
connect_tests()->

    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),  
   
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
    {ok,_}=resource_discovery_server:start(), 
    pong=rd:ping(),
    
    
    
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.
