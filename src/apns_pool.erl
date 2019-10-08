%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 八月 2018 10:19
%%%-------------------------------------------------------------------
-module(apns_pool).
-author("pei").

%% API
-export([push_notification_pool/2, push_notification_pool/3, push_notification_pool/4]).

-export([create_pool/2, create_pool/3, push_notification/3, push_notification/2, push_notification/1, connection_args/1]).

-include("apns.hrl").


create_pool(Settings, ConnArgs) ->
  create_pool(?DEFAULT_POOL_NAME, Settings, ConnArgs).

create_pool(PoolName, Settings, ConnArgs) when is_list(Settings)->
  PoolSize    = proplists:get_value(size, Settings, 5),
  MaxOverflow = proplists:get_value(max_overflow, Settings, 5),
  apns_pool_sup:add_pool(PoolName, [{name, {local, PoolName}},
    {worker_module, apns_pool_worker},
    {size, PoolSize},
    {max_overflow, MaxOverflow}, {strategy, fifo}], ConnArgs);
create_pool(PoolName, #{size := PoolSize, max_overflow := MaxOverflow}, ConnArgs)->
  apns_pool_sup:add_pool(PoolName, [{name, {local, PoolName}},
    {worker_module, apns_pool_worker},
    {size, PoolSize},
    {max_overflow, MaxOverflow}, {strategy, fifo}], ConnArgs).


push_notification({DeviceId, JSONMap, Headers}, {Then, Args}, Timeout) when is_function(Then, 2)->
  poolboy:transaction(?DEFAULT_POOL_NAME,
    fun(Pid)->
      apns_pool_worker:push_notification(Pid, {DeviceId, JSONMap, Headers}, {Then, Args})
    end, Timeout);
push_notification({DeviceId, JSONMap, Headers}, Then, Timeout)when is_function(Then, 1)->
  poolboy:transaction(?DEFAULT_POOL_NAME,
    fun(Pid)->
      apns_pool_worker:push_notification(Pid, {DeviceId, JSONMap, Headers}, Then)
    end, Timeout).

push_notification({DeviceId, JSONMap, Headers}, {Then, Args}) when is_function(Then, 2)->
  poolboy:transaction(?DEFAULT_POOL_NAME,
    fun(Pid)->
      apns_pool_worker:push_notification(Pid, {DeviceId, JSONMap, Headers}, {Then, Args})
    end);
push_notification({DeviceId, JSONMap, Headers}, Then)when is_function(Then, 1)->
  poolboy:transaction(?DEFAULT_POOL_NAME,
    fun(Pid)->
      apns_pool_worker:push_notification(Pid, {DeviceId, JSONMap, Headers}, Then)
    end).

push_notification({DeviceId, JSONMap, Headers})->
  poolboy:transaction(?DEFAULT_POOL_NAME,
    fun(Pid)->
      apns_pool_worker:push_notification(Pid, {DeviceId, JSONMap, Headers})
    end).


push_notification_pool(PoolName, {DeviceId, JSONMap, Headers}, {Then, Args}, Timeout) when is_function(Then, 2)->
  poolboy:transaction(PoolName,
    fun(Pid)->
      apns_pool_worker:push_notification(Pid, {DeviceId, JSONMap, Headers}, {Then, Args})
    end, Timeout);
push_notification_pool(PoolName, {DeviceId, JSONMap, Headers}, Then, Timeout)when is_function(Then, 1)->
  poolboy:transaction(PoolName,
    fun(Pid)->
      apns_pool_worker:push_notification(Pid, {DeviceId, JSONMap, Headers}, Then)
    end, Timeout).

push_notification_pool(PoolName, {DeviceId, JSONMap, Headers}, {Then, Args}) when is_function(Then, 2)->
  poolboy:transaction(PoolName,
    fun(Pid)->
      apns_pool_worker:push_notification(Pid, {DeviceId, JSONMap, Headers}, {Then, Args})
    end);
push_notification_pool(PoolName, {DeviceId, JSONMap, Headers}, Then)->
  poolboy:transaction(PoolName,
    fun(Pid)->
      apns_pool_worker:push_notification(Pid, {DeviceId, JSONMap, Headers}, Then)
    end).

push_notification_pool(PoolName, {DeviceId, JSONMap, Headers})->
  poolboy:transaction(PoolName,
    fun(Pid)->
      apns_pool_worker:push_notification(Pid, {DeviceId, JSONMap, Headers})
    end).

connection_args(Args) when is_list(Args)->
  #{ name => proplists:get_value(name, Args, undefined)
    , apple_host => proplists:get_value(apple_host, Args, "api.development.push.apple.com") %"api.development.push.apple.com"
    , apple_port => proplists:get_value(apple_port, Args, 443) %443
    , certfile   => full_file(proplists:get_value(certfile, Args)) %"priv/apns-dev-cert.pem"
    , keyfile    => full_file(proplists:get_value(keyfile, Args)) %"priv/apns-dev-key.pem"
    , timeout    => proplists:get_value(timeout, Args, 10000) %10000
    , password   => proplists:get_value(password, Args, "123456") %"123456"
    , type       => proplists:get_value(type, Args, cert) %cert
    , conn_life  => proplists:get_value(conn_life, Args, 180000)
  };
connection_args(Args = #{certfile := Certfile, keyfile := Keyfile})->
  Args#{certfile => full_file(Certfile), keyfile => full_file(Keyfile)}.

full_file({App, File})->
  filename:append(code:priv_dir(App), File);
full_file(F)->
  F.
