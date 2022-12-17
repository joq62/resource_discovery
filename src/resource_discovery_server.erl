%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(resource_discovery_server).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
% -include("").
%% --------------------------------------------------------------------
%% External exports

-export([
	 rpc_call/4,
	 rpc_call/5,
	 rpc_multicall/4,
	 rpc_multicall/5

	]).
% -export([ping/0]).

%% gen_server callbacks

-export([start/0,stop/0]).

-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {target_resource_types,  % I want part
	        local_resource_tuples,  % I have part
	        found_resource_tuples  % Local cache of found resources
	       }).

%% ====================================================================
%% External functions
%% ====================================================================
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


% ping()-> gen_server:call(?MODULE, {ping},infinity).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
       
    {ok,_}=heartbeat_server:start(),
    io:format(" Server started ~p~n",[{node(),?MODULE,?LINE}]),
    {ok, #state{target_resource_types = [],
	        local_resource_tuples = dict:new(),
		found_resource_tuples = dict:new()}
    }.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({fetch_resources,Type},_From, State) ->
    Reply=case dict:find(Type,State#state.found_resource_tuples) of
	      error->
		  {error,[eexists_resources]};
	      {ok,Resources}->
	%	  io:format("Fetch Resources ~p~n",[{node(),Type,Resources,?FUNCTION_NAME,?MODULE,?LINE}]),
		  {ok,Resources}
	  end,
    {reply, Reply, State};
handle_call( {delete_local_resource,Type,Resource},_From,State) ->
    ResourceTuples=State#state.found_resource_tuples,
  %  io:format("Delete ResourceTuples ~p~n",[{node(),dict:to_list(ResourceTuples),?FUNCTION_NAME,?MODULE,?LINE}]),   
    Reply=case delete_resource(Type,Resource,ResourceTuples) of
	      {error,Reason}->
		  NewState=State,
		  {error,Reason};
	      NewResourceTuples->
		 % io:format("Delete NewResourceTuples ~p~n",[{node(),dict:to_list(NewResourceTuples),?FUNCTION_NAME,?MODULE,?LINE}]),
		  NewState=State#state{found_resource_tuples=NewResourceTuples},
		  ok
	  end,
    
   
   

    {reply,Reply,NewState};


handle_call({get_state}, _From, State) ->
    Reply=[{target_resource_types,State#state.target_resource_types},
	   {local_resource_tuples,State#state.local_resource_tuples},
	   {found_resource_tuples, State#state.found_resource_tuples}],
    {reply, Reply, State};  

handle_call({ping}, _From, State) ->
    Reply=pong,
    {reply, Reply, State};  

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};


handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({add_target_resource_type,Type}, State) ->
    TargetTypes=State#state.target_resource_types,
    NewTargetTypes=[Type|lists:delete(Type,TargetTypes)],
%    io:format("NewTargetTypes ~p~n",[{node(),NewTargetTypes,?FUNCTION_NAME,?MODULE,?LINE}]),
    {noreply, State#state{target_resource_types=NewTargetTypes}};

handle_cast( {add_local_resource,Type,Resource}, State) ->
    ResourceTuples=State#state.local_resource_tuples,
    NewResourceTuples=add_resource(Type,Resource,ResourceTuples),
%    io:format("NewResourceTuples ~p~n",[{node(),dict:to_list(NewResourceTuples),?FUNCTION_NAME,?MODULE,?LINE}]),
    {noreply, State#state{local_resource_tuples=NewResourceTuples}};

%handle_cast( {delete_local_resource,Type,Resource}, State) ->
%    ResourceTuples=State#state.local_resource_tuples,
%    NewResourceTuples=delete_resource(Type,Resource,ResourceTuples),
 %   io:format("NewResourceTuples ~p~n",[{node(),dict:to_list(NewResourceTuples),?FUNCTION_NAME,?MODULE,?LINE}]),
%    {noreply, State#state{local_resource_tuples=NewResourceTuples}};


handle_cast({trade_resources}, State) ->
    ResourceTuples=State#state.local_resource_tuples,
    AllNodes =[node()|nodes()],
    lists:foreach(
      fun(Node) ->
	      gen_server:cast({?MODULE,Node},
			     {trade_resources, {node(),ResourceTuples}})
      end,
      AllNodes),
    {noreply, State};

handle_cast({trade_resources, {ReplyTo,Remotes}},
	    #state{local_resource_tuples=Locals,
		   target_resource_types=TargetTypes,
		   found_resource_tuples = OldFound} =State) ->
    
%    io:format("ReplyTo,Remotes ~p~n",[{node(),ReplyTo,dict:to_list(Remotes),?FUNCTION_NAME,?MODULE,?LINE}]),

    FilteredRemotes=resources_for_types(TargetTypes,Remotes),
    NewFound= add_resources(FilteredRemotes,OldFound),
%    io:format("OldFound ~p~n",[{node(),dict:to_list(OldFound),?FUNCTION_NAME,?MODULE,?LINE}]),
%    io:format("NewFound ~p~n",[{node(),dict:to_list(NewFound),?FUNCTION_NAME,?MODULE,?LINE}]),
    case ReplyTo of
        noreply ->
	    ok;
	_ ->
	   gen_server:cast({?MODULE,ReplyTo},
			   {trade_resources, {noreply, Locals}})
    end,
    {noreply, State#state{found_resource_tuples=NewFound}};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,?LINE,time()}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Exported functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
add_resources([{Type,Resource}|T],ResourceTuples)->
    add_resources(T,add_resource(Type,Resource,ResourceTuples));
add_resources([],ResourceTuples) ->
    ResourceTuples.

add_resource(Type,Resource,ResourceTuples)->
    case dict:find(Type,ResourceTuples) of
	{ok,ResourceList}->
	    NewList=[Resource|lists:delete(Resource,ResourceList)],
	    dict:store(Type,NewList,ResourceTuples);
	error ->
	    dict:store(Type,[Resource],ResourceTuples)
    end.

resources_for_types(Types,ResourceTuples)->
    Fun =
	fun(Type,Acc) ->
		case dict:find(Type,ResourceTuples) of
		    {ok,List}->
			[{Type, Resource} || Resource <- List] ++ Acc;
		    error ->
			Acc
		end
	end,
    lists:foldl(Fun,[],Types).

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
delete_resource(Type,Resource,ResourceTuples)->
    Result=case dict:find(Type,ResourceTuples) of
	       {ok,ResourceList}->
		   NewList=lists:delete(Resource,ResourceList),
		   dict:store(Type,NewList,ResourceTuples);
	       error ->
		   {error,[eexists,Type,?MODULE,?FUNCTION_NAME,?LINE]}
						% ResourceTuples
	   end,    
    Result.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
rpc_call(Type, Module, Function, Args, Timeout) ->
    Reply=case rd:fetch_resources(Type) of
	      {ok,[]}->
		  {error,[eexists_resources]};
	      {ok, Resources} ->
		  [Resource|_]=Resources,
		  rpc:call(Resource, Module, Function, Args, Timeout);
	%	  case rpc:call(Resource, Module, Function, Args, Timeout) of
	%	      {badrpc, _Reason} ->
	%		  case rd:delete_local_resource(Type, Resource) of
	%		      {error,Reason}->
	%			  {error,Reason};
	%		      ok->
	%			  rd:rpc_call(Type, Module, Function, Args, Timeout)
	%		  end;
	%	      R ->
	%		  R
	%	  end;
	      {error,Reason}->
		  {error,Reason}
	  end,
    Reply.

rpc_call(Type, Module, Function, Args) ->
    rpc_call(Type, Module, Function, Args, 60000).

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
rpc_multicall(Type, Module, Function, Args, Timeout) ->
    Result=case  rd:fetch_resources(Type) of
	       {error,Reason} -> 
		   {error,Reason};
	       {ok, Resources} -> 
		   {Resl, BadNodes} = rpc:multicall(Resources, Module, Function, Args, Timeout),
	%	   [rd:delete_local_resource(Type, BadNode) || BadNode <- BadNodes],
		   {Resl, BadNodes}
	   end,
    Result.

rpc_multicall(Type, Module, Function, Args) ->
    rpc_multicall(Type, Module, Function, Args, 60000).
