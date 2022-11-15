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
-module(basic_tests).      
 
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
    
    Local0=[{type0,node0},{type1,node0}],
    Local1=[{type1,node1}],
    Local2=[{type2,node2},{type21,node2}],
    Target0=[type1,type2],
    Target1=[type0,type21],
    Target2=[type1,type2],
    [Node0,Node1,Node2]=lists:sort(test_nodes:get_nodes()),
    Date=date(),
    {error,[eexists,type0,handle_call,resource_discovery_server,_]}=rpc:call(Node0,rd,rpc_multicall,[type0,erlang,date,[]]),  
    {[Date],[]}=rpc:call(Node1,rd,rpc_multicall,[type0,erlang,date,[]]),  
    {error,[eexists,type0,handle_call,resource_discovery_server,_]}=rpc:call(Node2,rd,rpc_multicall,[type0,erlang,date,[]]),  
    
    {[Date,Date],[]}=rpc:call(Node0,rd,rpc_multicall,[type1,erlang,date,[]]),
    {error,[eexists,type1,handle_call,resource_discovery_server,_]}=rpc:call(Node1,rd,rpc_multicall,[type1,erlang,date,[]]),
    {[Date,Date],[]}=rpc:call(Node2,rd,rpc_multicall,[type1,erlang,date,[]]),
    {[Date],[]}=rpc:call(Node0,rd,rpc_multicall,[type2,erlang,date,[]]),
    
    

    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
rpc_tests()->

    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]), 
    

    Local0=[{type0,node0},{type1,node0}],
    Local1=[{type1,node1}],
    Local2=[{type2,node2},{type21,node2}],
    Target0=[type1,type2],
    Target1=[type0,type21],
    Target2=[type1,type2],
    [Node0,Node1,Node2]=lists:sort(test_nodes:get_nodes()),
    Date=date(),
    {error,[eexists,type0,handle_call,resource_discovery_server,_]}=rpc:call(Node0,rd,rpc_call,[type0,erlang,date,[]]),  
    Date=rpc:call(Node1,rd,rpc_call,[type0,erlang,date,[]]),  
    {error,[eexists,type0,handle_call,resource_discovery_server,_]}=rpc:call(Node2,rd,rpc_call,[type0,erlang,date,[]]),  
    
    Date=rpc:call(Node0,rd,rpc_call,[type1,erlang,date,[]]),
    {error,[eexists,type1,handle_call,resource_discovery_server,_]}=rpc:call(Node1,rd,rpc_call,[type1,erlang,date,[]]),
    Date=rpc:call(Node2,rd,rpc_call,[type1,erlang,date,[]]),
    Date=rpc:call(Node0,rd,rpc_call,[type2,erlang,date,[]]),
    

    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
stop_start_tests()->

    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]), 
 
    [Node0,Node1,Node2]=lists:sort(test_nodes:get_nodes()),
    Date=date(),

    {[Date,Date],[]}=rpc:call(Node2,rd,rpc_multicall,[type1,erlang,date,[]]),
    Date=rpc:call(Node2,rd,rpc_call,[type1,erlang,date,[]]),
    rpc:call(Node1,init,stop,[]),
    timer:sleep(2000),
    
    {[Date],[Node1]}=rpc:call(Node2,rd,rpc_multicall,[type1,erlang,date,[]]),
    Date=rpc:call(Node2,rd,rpc_call,[type1,erlang,date,[]]),
    rpc:call(Node0,init,stop,[]),
    timer:sleep(2000),
    {[],[Node0]}=rpc:call(Node2,rd,rpc_multicall,[type1,erlang,date,[]]),
    {error,[no_resources,type1,resource_discovery_server,rpc_call,_]}=rpc:call(Node2,rd,rpc_call,[type1,erlang,date,[]]),
    

    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
normal_tests()->

    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]), 
 
    [Node0,Node1,Node2]=lists:sort(test_nodes:get_nodes()),
    Local0=[{type0,Node0},{type1,Node0}],Local1=[{type1,Node1}],Local2=[{type2,Node2},{type21,Node2}],
    Target0=[type1,type2],Target1=[type0,type21],Target2=[type1,type2],
    % Add Node0
    [rpc:call(Node0,rd,add_local_resource,[Type,Instance],5000)||{Type,Instance}<-Local0],
    [rpc:call(Node0,rd,add_target_resource_type,[Type],5000)||Type<-Target0],
    ok=rpc:call(Node0,rd,trade_resources,[],5000),

    [rpc:call(Node1,rd,add_local_resource,[Type,Instance],5000)||{Type,Instance}<-Local1],
    [rpc:call(Node1,rd,add_target_resource_type,[Type],5000)||Type<-Target1],
    ok=rpc:call(Node1,rd,trade_resources,[],5000),

    [rpc:call(Node2,rd,add_local_resource,[Type,Instance],5000)||{Type,Instance}<-Local2],
    [rpc:call(Node2,rd,add_target_resource_type,[Type],5000)||Type<-Target2],
    ok=rpc:call(Node2,rd,trade_resources,[],5000),
    
    timer:sleep(3000),
    {ok,[_,_]}=rpc:call(Node0,rd,fetch_resources,[type1],5000),
    
    
    

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
    ok=test_nodes:start_nodes(),
    [Node0,Node1,Node2]=test_nodes:get_nodes(),
    [Node0,Node1,Node2]=lists:sort([Node0,Node1,Node2]),
    [rpc:call(N1,net_adm,ping,[N2])||N1<-[Node0,Node1,Node2],
				     N2<-[Node0,Node1,Node2]],
    [rpc:call(N,code,add_patha,[ebin],2000)||N<-test_nodes:get_nodes()],
    [rpc:call(N,resource_discovery_server,start,[],2000)||N<-test_nodes:get_nodes()],
    [pong,pong,pong]=[rpc:call(N,resource_discovery_server,ping,[],2000)||N<-test_nodes:get_nodes()],
    
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.
