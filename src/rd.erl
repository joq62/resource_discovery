%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(rd).

-behaviour(gen_server).
%%--------------------------------------------------------------------
-include("log.api").
%%--------------------------------------------------------------------

-type resource_type() :: atom(). % gen_server or lib module
-type resource() :: node().      % node()
%-type resource_tuple() :: {resource_type(), resource()}. % {Module,Node} 

%% API
-export([add_target_resource_type/1,
	 add_local_resource/2,
	 delete_local_resource/2,	 
	 fetch_resources/1,
	 trade_resources/0
	]).

-export([
	 rpc_call/4,
	 rpc_call/5,
	 rpc_multicall/4,
	 rpc_multicall/5
	]).


-export([
	 ping/0
	]).

-export([start_link/0,
	 start/0,
	 stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {target_resource_types,  % I want part
	        local_resource_tuples,  % I have part
	        found_resource_tuples  % Local cache of found resources
	       }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
add_target_resource_type(ResourceType) ->
    gen_server:cast(?SERVER, {add_target_resource_type, ResourceType}).

add_local_resource(ResourceType, Resource) ->
    gen_server:cast(?SERVER, {add_local_resource, ResourceType, Resource}).
delete_local_resource(ResourceType, Resource) ->
    gen_server:call(?SERVER, {delete_local_resource, ResourceType, Resource}).

fetch_resources(ResourceType) ->
    gen_server:call(?SERVER, {fetch_resources, ResourceType}).

trade_resources() ->
    gen_server:cast(?SERVER,{trade_resources}).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

start()->
    application:start(?MODULE).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


stop()-> gen_server:call(?SERVER, {stop},infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.

init([]) ->
    %  {ok,_}=heartbeat_server:start(),
   % ?LOG_NOTICE("Server started ",[]),
    
    {ok, #state{target_resource_types = [],
	        local_resource_tuples = dict:new(),
		found_resource_tuples = dict:new()}}.

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
		  Resources
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


handle_call(UnMatchedSignal, From, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({add_target_resource_type,ResourceType}, State) ->
    TargetTypes=State#state.target_resource_types,
    NewTargetTypes=[ResourceType|lists:delete(ResourceType,TargetTypes)],
%    io:format("NewTargetTypes ~p~n",[{node(),NewTargetTypes,?FUNCTION_NAME,?MODULE,?LINE}]),
    {noreply, State#state{target_resource_types=NewTargetTypes}};

handle_cast( {add_local_resource,ResourceType,Resource}, State) ->
    ResourceTuples=State#state.local_resource_tuples,
    NewResourceTuples=add_resource(ResourceType,Resource,ResourceTuples),
%    io:format("NewResourceTuples ~p~n",[{node(),dict:to_list(NewResourceTuples),?FUNCTION_NAME,?MODULE,?LINE}]),
    {noreply, State#state{local_resource_tuples=NewResourceTuples}};

%handle_cast( {delete_local_resource,ResourceType,Resource}, State) ->
%    ResourceTuples=State#state.local_resource_tuples,
%    NewResourceTuples=delete_resource(ResourceType,Resource,ResourceTuples),
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


handle_cast(UnMatchedSignal, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(Info, State) ->
    io:format("unmatched_signal ~p~n",[{Info,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
add_resources([{ResourceType,Resource}|T],ResourceTuples)->
    add_resources(T,add_resource(ResourceType,Resource,ResourceTuples));
add_resources([],ResourceTuples) ->
    ResourceTuples.

add_resource(ResourceType,Resource,ResourceTuples)->
    case dict:find(ResourceType,ResourceTuples) of
	{ok,ResourceList}->
	    NewList=[Resource|lists:delete(Resource,ResourceList)],
	    dict:store(ResourceType,NewList,ResourceTuples);
	error ->
	    dict:store(ResourceType,[Resource],ResourceTuples)
    end.

resources_for_types(ResourceTypes,ResourceTuples)->
    Fun =
	fun(ResourceType,Acc) ->
		case dict:find(ResourceType,ResourceTuples) of
		    {ok,List}->
			[{ResourceType, Resource} || Resource <- List] ++ Acc;
		    error ->
			Acc
		end
	end,
    lists:foldl(Fun,[],ResourceTypes).

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
delete_resource(ResourceType,Resource,ResourceTuples)->
    Result=case dict:find(ResourceType,ResourceTuples) of
	       {ok,ResourceList}->
		   NewList=lists:delete(Resource,ResourceList),
		   dict:store(ResourceType,NewList,ResourceTuples);
	       error ->
		   {error,[eexists,ResourceType,?MODULE,?FUNCTION_NAME,?LINE]}
						% ResourceTuples
	   end,    
    Result.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
rpc_call(ResourceType, Module, Function, Args, Timeout) ->
    Reply=case rd:fetch_resources(ResourceType) of
	      {error,Reason}->
		  {error,Reason};
	      []->
		  {error,[eexists_resources]};
	      Resources ->
		  [Resource|_]=Resources,
		  rpc:call(Resource, Module, Function, Args, Timeout)
	%	  case rpc:call(Resource, Module, Function, Args, Timeout) of
	%	      {badrpc, _Reason} ->
	%		  case rd:delete_local_resource(ResourceType, Resource) of
	%		      {error,Reason}->
	%			  {error,Reason};
	%		      ok->
	%			  rd:rpc_call(ResourceType, Module, Function, Args, Timeout)
	%		  end;
	%	      R ->
	%		  R
	%	  end;
	     
	  end,
    Reply.

rpc_call(ResourceType, Module, Function, Args) ->
    rpc_call(ResourceType, Module, Function, Args, 60000).

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
rpc_multicall(ResourceType, Module, Function, Args, Timeout) ->
    Result=case  rd:fetch_resources(ResourceType) of
	       {error,Reason} -> 
		   {error,Reason};
	       Resources -> 
		   {Resl, BadNodes} = rpc:multicall(Resources, Module, Function, Args, Timeout),
	%	   [rd:delete_local_resource(ResourceType, BadNode) || BadNode <- BadNodes],
		   {Resl, BadNodes}
	   end,
    Result.

rpc_multicall(ResourceType, Module, Function, Args) ->
    rpc_multicall(ResourceType, Module, Function, Args, 60000).
