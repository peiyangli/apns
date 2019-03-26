%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 八月 2018 9:16
%%%-------------------------------------------------------------------
-module(apns_sup).
-author("pei").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->

  SupFlags = {one_for_one, 1000, 3600},

  ApnsConn = {'apns_connection_sup', {'apns_connection_sup', start_link, []},
    permanent, 2000, supervisor, ['apns_connection_sup']},

  ApnsPool = {'apns_pool_sup', {'apns_pool_sup', start_link, []},
    permanent, 2000, supervisor, ['apns_pool_sup']},

  {ok, {SupFlags, [ApnsConn, ApnsPool]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
