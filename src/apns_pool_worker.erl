%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 八月 2018 9:23
%%%-------------------------------------------------------------------
-module(apns_pool_worker).
-author("pei").

-behaviour(gen_server).

%% API
-export([start_link/1, on/3, on/2, push_notification/2, push_notification/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%-record(state, {cid}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
  gen_server:start_link(?MODULE, [Args], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Args]) when is_list(Args) ->
  ConnArgs = apns_pool:connection_args(Args),
  init([ConnArgs]);

init([ConnArgs]) when is_map(ConnArgs) ->
%%  {ok, ConnectionId} = apns:connect(ConnArgs),
  {ok, #{cags => ConnArgs,  cid => undefined}}.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({on, _} = Request, #{cid:=undefined, cags := ConnArgs} = State)->
  {ok, ConnectionId} = apns:connect(ConnArgs),
  handle_cast(Request, State#{cid=>ConnectionId});
handle_cast({on, {Fun, Args}}, #{cags := #{conn_life:=ConnLife}} = State)->
  try
    Fun(State, Args)
  catch
    Class:Reason:Stack ->
      lager:error("[apns]Class: ~p, Reason: ~p, Stack: ~p~n", [Class, Reason, Stack])
  end,
  {noreply, State, ConnLife};
handle_cast({on, Fun}, #{cags := #{conn_life:=ConnLife}} =State)->
  try
    Fun(State)
  catch
    Class:Reason:Stack ->
      lager:error("[apns]Class: ~p, Reason: ~p, Stack: ~p~n", [Class, Reason, Stack])
  end,
  {noreply, State, ConnLife};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(timeout,  #{cid := ConnectionId} = State) when is_pid(ConnectionId) ->
  apns:close_connection(ConnectionId),
  lager:info("[]timeout for close connection~n", []),
  {noreply, State#{cid => undefined}};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #{cid := ConnectionId}) when is_pid(ConnectionId)->
  apns:close_connection(ConnectionId),
  ok;
terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

push_notification(Pid, {DeviceId, JSONMap, Headers})->
  on(Pid, fun
            (#{cid:=ConntionID})->
              apns:push_notification(ConntionID, DeviceId, JSONMap, Headers)
          end).

push_notification(Pid, {DeviceId, JSONMap, Headers}, {Then, Args}) when is_function(Then, 2)->
  on(Pid, fun
            (#{cid:=ConntionID})->
              Rt = apns:push_notification(ConntionID, DeviceId, JSONMap, Headers),
              Then(Rt, Args)
          end);
push_notification(Pid, {DeviceId, JSONMap, Headers}, Then) when is_function(Then, 1)->
  on(Pid, fun
            (#{cid:=ConntionID})->
              Rt = apns:push_notification(ConntionID, DeviceId, JSONMap, Headers),
              Then(Rt)
          end).

on(Pid, Fun, Args) when is_function(Fun, 2)->
  gen_server:cast(Pid, {on, {Fun, Args}}).
on(Pid, Fun) when is_function(Fun, 1)->
  gen_server:cast(Pid, {on, Fun}).

