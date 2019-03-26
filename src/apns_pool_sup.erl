%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 八月 2018 9:22
%%%-------------------------------------------------------------------
-module(apns_pool_sup).
-author("pei").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1, add_pool/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, Pools} = application:get_env(apns, pools),
  PoolSpec = lists:map(fun
                         ({PoolName, SizeArgs, Args}) ->
                           PoolArgs = [{name, {local, PoolName}},
                            {worker_module, apns_pool_worker}] ++ SizeArgs,

                           ConnArgs = apns_pool:connection_args(Args),

                           io:format("[ConnArgs]~p~n", [ConnArgs]),
                           poolboy:child_spec(PoolName, PoolArgs, ConnArgs)
                       end, Pools),
  {ok, { {one_for_one, 10, 10}, PoolSpec} }.

add_pool(Name, PoolArgs, Args) when is_map(Args) ->
  ChildSpec = poolboy:child_spec(Name, PoolArgs, Args),
  supervisor:start_child(?MODULE, ChildSpec).
