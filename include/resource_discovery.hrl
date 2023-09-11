%%% Type Definitions
%% resource_typ: service module   
%% resource: Node
%% resource_tuple: {{lgh_home_automation,"1.0.0"},[{ServerModule,Node1}}
%% {ResourceType1,[Node1,Node2..]}
%% 


-type resource_type() :: atom().
-type resource() :: term().
-type resource_tuple() :: {resource_type(), resource()}.
-type needed_types() ::[resource_type()].
-type exposed_types() ::[resource_type()].
-type cached_resources() :: [resource_tuple()].


