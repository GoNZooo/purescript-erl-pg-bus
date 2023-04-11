-module(pgBus@foreign).

-define(SUBSCRIPTION_NAME(BusName, Pid), {?MODULE, BusName, Pid}).
-define(UNSUBSCRIBE_MESSAGE, 'pg_bus_unsubscribe$').
-define(PUBLISHED_MESSAGE(Message), {'pg_bus_publish$', Message}).

-export([subscribe/3, unsubscribe/2, publish/2]).

subscribe(BusName, F, Pid) ->
  fun() ->
     case subscription_exists(BusName, Pid) of
       true -> unit;
       false ->
         SubscriptionPid = spawn(fun() -> subscribe_loop(Pid, F) end),
         pg:join(BusName, SubscriptionPid),
         case register_subscription_name(BusName, Pid, SubscriptionPid) of
           {ok, _SubscriptionName} -> unit;
           {error, {already_registered, _SubscriptionName}} -> unit
         end
     end
  end.

unsubscribe(BusName, Pid) ->
  fun() ->
     SubscriptionName = ?SUBSCRIPTION_NAME(BusName, Pid),
     case global:whereis_name(SubscriptionName) of
       SubscriptionPid when is_pid(SubscriptionPid) -> SubscriptionPid ! ?UNSUBSCRIBE_MESSAGE;
       undefined -> unit
     end,
     unit
  end.

publish(BusName, Message) ->
  fun() ->
     MemberPids = pg:get_members(BusName),
     lists:foreach(fun(Pid) -> Pid ! ?PUBLISHED_MESSAGE(Message) end, MemberPids)
  end.

%%% Internals

subscribe_loop(Pid, F) ->
  receive
    ?UNSUBSCRIBE_MESSAGE ->
      ok;
    ?PUBLISHED_MESSAGE(Message) ->
      Pid ! F(Message),
      subscribe_loop(Pid, F)
  end.

register_subscription_name(BusName, Pid, SubscriptionPid) ->
  SubscriptionName = ?SUBSCRIPTION_NAME(BusName, Pid),
  case global:register_name(SubscriptionName, SubscriptionPid) of
    yes ->
      {ok, SubscriptionName};
    no ->
      {error, {already_registered, SubscriptionName}}
  end.

subscription_exists(BusName, Pid) ->
  SubscriptionName = ?SUBSCRIPTION_NAME(BusName, Pid),
  case global:whereis_name(SubscriptionName) of
    SubscriptionPid when is_pid(SubscriptionPid) ->
      true;
    undefined ->
      false
  end.
