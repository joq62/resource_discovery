%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(rd_server).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
% -include("").
%% --------------------------------------------------------------------
%% External exports
-export([add_target_resource_type/1,
	 add_local_resource/2,
	 fetch_resources/1,
	 trade_resources/0
	]).

-export([ensure_contact/0]).

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


add_target_resource_type(Type)->
    gen_server:cast(?MODULE, {add_target_resource_type,Type}). 
add_local_resource(Type,Instance)->
    gen_server:cast(?MODULE, {add_local_resource,Type,Instance}). 
	 
fetch_resources(Type)->
    gen_server:call(?MODULE, {fetch_resources,Type},infinity).

trade_resources()->
    gen_server:cast(?MODULE, {trade_resources}). 

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
		  {error,[eexists,Type,?FUNCTION_NAME,?MODULE,?LINE]};
	      {ok,Resources}->
		  Resources
	  end,
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
%    io:format("NewTargetTypes ~p~n",[{NewTargetTypes,?FUNCTION_NAME,?MODULE,?LINE}]),
    {noreply, State#state{target_resource_types=NewTargetTypes}};

handle_cast( {add_local_resource,Type,Instance}, State) ->
    ResourceTuples=State#state.local_resource_tuples,
    NewResourceTuples=add_resource(Type,Instance,ResourceTuples),
%    io:format("NewResourceTuples ~p~n",[{NewResourceTuples,?FUNCTION_NAME,?MODULE,?LINE}]),
    {noreply, State#state{local_resource_tuples=NewResourceTuples}};


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
    
    FilteredRemotes=resources_for_types(TargetTypes,Remotes),
    NewFound= add_resources(FilteredRemotes,OldFound),
%    io:format("NewFound ~p~n",[{NewFound,?FUNCTION_NAME,?MODULE,?LINE}]),
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
ensure_contact()->
    DefaultNodes=['c0@c0','c2@c2','joq62-X550CA@joq62-X550CA','test@joq62-X550CA'],
    DefaultNodes.    

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
add_resource(Type,Resource,ResourceTuples)->
    case dict:find(Type,ResourceTuples) of
	{ok,ResourceList}->
	    NewList=[Resource|lists:delete(Resource,ResourceList)],
	    dict:store(Type,NewList,ResourceTuples);
	error ->
	    dict:store(Type,[Resource],ResourceTuples)
    end.

add_resources([{Type,Resource}|T],ResourceTuples)->
    add_resources(T,add_resource(Type,Resource,ResourceTuples));
add_resources([],ResourceTuples) ->
    ResourceTuples.

resources_for_types(Types,ResourceTuples)->
    Fun =
	fun(Type,Acc) ->
		case dict:find(Type,ResourceTuples) of
		    {ok,List}->
			[{Type,Instance}||Instance <- List] ++Acc;
		    error ->
			Acc
		end
	end,
    lists:foldl(Fun,[],Types).
