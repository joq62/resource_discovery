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
%    ok=rpc_multi_tests(),
    ok=stop_start_tests(),
   

    io:format("Succeeded !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
 
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
    {[Date],[]}=rpc:call(Node0,rd,rpc_multicall,[type0,erlang,date,[]]),  
    {[],[]}=rpc:call(Node1,rd,rpc_multicall,[type0,erlang,date,[]]),  
    {[],[]}=rpc:call(Node2,rd,rpc_multicall,[type0,erlang,date,[]]),  
    
    {[Date],[]}=rpc:call(Node0,rd,rpc_multicall,[type1,erlang,date,[]]),
    {[Date],[]}=rpc:call(Node1,rd,rpc_multicall,[type1,erlang,date,[]]),
    {[],[]}=rpc:call(Node2,rd,rpc_multicall,[type1,erlang,date,[]]),
    {[],[]}=rpc:call(Node0,rd,rpc_multicall,[type2,erlang,date,[]]),
    
    

    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
rpc_tests()->

    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]), 
    
    [Node0,Node1,Node2]=lists:sort(test_nodes:get_nodes()),
    Date=date(), 
    Local0=[{type0,{Node0,erlang}},{type1,{Node0,filelib}}],
    Local1=[{type1,{Node1,filelib}}],
    Local2=[{type2,{Node2,test_module2}},{type0,{Node2,filelib}}],
    Target0=[type1,type2],
    Target1=[type0,type2],
    Target2=[type1,type2],

 
    Date=rpc:call(Node0,rd,call,[type1,erlang,date,[],5000]),  
    Date=rpc:call(Node1,rd,call,[type0,date,[],5000]),  
    {error,[eexists_resources]}=rpc:call(Node2,rd,call,[type0,is_file,["Makefile"],5000]),  
    
    Date=rpc:call(Node0,rd,call,[type1,erlang,date,[],5000]),
    {error,[eexists_resources]}=rpc:call(Node1,rd,call,[type1,erlang,date,[],5000]),
    Date=rpc:call(Node2,rd,call,[type1,erlang,date,[],5000]),
    42=rpc:call(Node1,rd,call,[type2,test_module2,add,[22,20],5000]),
    42=rpc:call(Node1,rd,call,[type2,add,[22,20],5000]),
    

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
    [pong,pong,pong]=[net_adm:ping(N)||N<- [Node0,Node1,Node2]],
 
    Local0=[{type0,{Node0,erlang}},{type1,{Node0,filelib}}],
    Local1=[{type1,{Node1,filelib}}],
    Local2=[{type2,{Node2,test_module2}},{type0,{Node2,filelib}}],
    Target0=[type1,type2],
    Target1=[type0,type2],
    Target2=[type1,type2],

   [{'n0@c50',filelib},{'n1@c50',filelib}]=lists:sort(rpc:call(Node0,rd,fetch_resources,[type1])),
    []=lists:sort(rpc:call(Node1,rd,fetch_resources,[type1])),
    []=lists:sort(rpc:call(Node2,rd,fetch_resources,[type0])),
    
    %% kill Node0 => Node1 will not get type0 and Node2 type1 from Node1 
    ok=slave:stop(Node0),
    timer:sleep(2000),
    rpc:call(Node1,rd,trade_resources,[],5000),
    timer:sleep(5000),
    
    [{'n2@c50',test_module2}]=lists:sort(rpc:call(Node1,rd,fetch_resources,[type2])),
    [{'n0@c50',filelib},{'n1@c50',filelib}]=lists:sort(rpc:call(Node2,rd,fetch_resources,[type1])),    

    
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
    % type0 -> Node0,Node2
    % type1 -> Node0,Node1
    % type2 -> Node2
    Local0=[{type0,{Node0,erlang}},{type1,{Node0,filelib}}],
    Local1=[{type1,{Node1,filelib}}],
    Local2=[{type2,{Node2,test_module2}},{type0,{Node2,filelib}}],
    
    % Node0(type1)-> [Node0,Node1]
    % Node0((type2) -> Node2
    % Node1(type0)-> [Node0,Node2]
    % Node1(type2)-> [Node2]
    % Node2(type1)-> [Node0,Node1]
    % Node2(type2)-> [Node2]
    Target0=[type1,type2],
    Target1=[type0,type2],
    Target2=[type1,type2],
   
    Date=erlang:date(),

   % Add Node0
    ok=rpc:call(Node0,rd,start,[]),
    pong=rpc:call(Node0,rd,ping,[]),
    [ok=rpc:call(Node0,rd,add_local_resource,[Type,Instance],5000)||{Type,Instance}<-Local0],
    [ok=rpc:call(Node0,rd,add_target_resource_type,[Type],5000)||Type<-Target0],
    ok=rpc:call(Node0,rd,trade_resources,[],5000),

    timer:sleep(5000),
  

    [{Node0,filelib}]=rpc:call(Node0,rd,fetch_resources,[type1],5000),
    []=rpc:call(Node0,rd,fetch_resources,[type2],5000),
   
    
    Date=rpc:call(Node0,rd,call,[type1,erlang,date,[],5000],6000),  
    true=rpc:call(Node0,rd,call,[type1,is_file,["Makefile"],5000],6000),
  
  
   %%% Node1
    ok=rpc:call(Node1,rd,start,[]),
    pong=rpc:call(Node1,rd,ping,[]),
    [ok=rpc:call(Node1,rd,add_local_resource,[Type,Instance],5000)||{Type,Instance}<-Local1],
    [ok=rpc:call(Node1,rd,add_target_resource_type,[Type],5000)||Type<-Target1],
    ok=rpc:call(Node1,rd,trade_resources,[],5000),
 
    timer:sleep(5000),
    [{'n0@c50',filelib},{'n1@c50',filelib}]=lists:sort(rpc:call(Node0,rd,fetch_resources,[type1],5000)), 
    []=lists:sort(rpc:call(Node1,rd,fetch_resources,[type1],5000)),
    [{'n0@c50',erlang}]=lists:sort(rpc:call(Node1,rd,fetch_resources,[type0],5000)),
    []=rpc:call(Node0,rd,fetch_resources,[type2],5000),

    %% Node2
    ok=rpc:call(Node2,rd,start,[]),
    pong=rpc:call(Node2,rd,ping,[]),
    [rpc:call(Node2,rd,add_local_resource,[Type,Instance],5000)||{Type,Instance}<-Local2],
    [rpc:call(Node2,rd,add_target_resource_type,[Type],5000)||Type<-Target2],
    ok=rpc:call(Node2,rd,trade_resources,[],5000),
    
    timer:sleep(3000),
    [{'n0@c50',filelib},{'n1@c50',filelib}]=lists:sort(rpc:call(Node0,rd,fetch_resources,[type1],5000)), 
    []=lists:sort(rpc:call(Node1,rd,fetch_resources,[type1],5000)),
    [{'n0@c50',erlang},{'n2@c50',filelib}]=lists:sort(rpc:call(Node1,rd,fetch_resources,[type0],5000)),
    [{'n0@c50',filelib},{'n1@c50',filelib}]=lists:sort(rpc:call(Node2,rd,fetch_resources,[type1],5000)),
    
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
    [pong,pong,pong,pong,pong,
     pong,pong,pong,pong]=[rpc:call(N1,net_adm,ping,[N2])||N1<-[Node0,Node1,Node2],
				     N2<-[Node0,Node1,Node2]],
  %  [true,true,true]=[rpc:call(N,file,get_cwd,[],2000)||N<-test_nodes:get_nodes()],
    [true,true,true]=[rpc:call(N,code,add_patha,["test_ebin"],2000)||N<-[Node0,Node1,Node2]],
    [true,true,true]=[rpc:call(N,code,add_patha,["ebin"],2000)||N<-[Node0,Node1,Node2]],
%    [ok,ok,ok]=[rpc:call(N,rd,start,[],2000)||N<-[Node0,Node1,Node2]],

  %  [pong,pong,pong,pong,pong,
  %   pong,pong,pong,pong]=[rpc:call(N1,net_adm,ping,[N2])||N1<-[Node0,Node1,Node2],
%				     N2<-[Node0,Node1,Node2]],

    [Node0,Node1,Node2]=[rpc:call(N,erlang,node,[],2000)||N<-[Node0,Node1,Node2]],
   % [Node0,Node1,Node2]=[rpc:call(N,rd,trade_resources,[],2000)||N<-[Node0,Node1,Node2]],

 %   [pong,pong,pong]=[rpc:call(Nx,rd,ping,[],2000)||Nx<-[Node0,Node1,Node2]], 
    
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.
