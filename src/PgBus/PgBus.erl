-module(pgBus@foreign).

-define(SUBSCRIPTION_NAME(Name, Pid), {?MODULE, Name, Pid}).
-define(UNSUBSCRIBE_MESSAGE, 'pg_bus_unsubscribe$').
-define(PUBLISHED_MESSAGE(Message), {'pg_bus_publish$', Message}).

-export([subscribe/3, unsubscribe/2, publish/2]).

subscribe(Name, F, Pid) ->
  fun() ->
     case subscription_exists(Name, Pid) of
       true -> unit;
       false ->
         SubscriptionPid = spawn(fun() -> subscribe_loop(Pid, F) end),
         case register_subscription_name(Name, Pid, SubscriptionPid) of
           {ok, _SubscriptionName} -> unit;
           {error, {already_registered, _SubscriptionName}} -> unit
         end
     end
  end.

unsubscribe(Name, Pid) ->
  fun() ->
     SubscriptionName = ?SUBSCRIPTION_NAME(Name, Pid),
     case global:whereis_name(SubscriptionName) of
       SubscriptionPid when is_pid(SubscriptionPid) -> SubscriptionPid ! ?UNSUBSCRIBE_MESSAGE;
       undefined -> unit
     end,
     unit
  end.

publish(Name, Message) ->
  fun() ->
     SubscriptionName = ?SUBSCRIPTION_NAME(Name, self()),
     case global:whereis_name(SubscriptionName) of
       Pid when is_pid(Pid) -> Pid ! ?PUBLISHED_MESSAGE(Message);
       undefined -> unit
     end,
     unit
  end.

%%% Internals

subscribe_loop(Pid, F) ->
  receive
    ?UNSUBSCRIBE_MESSAGE ->
      ok;
    ?PUBLISHED_MESSAGE(Message) ->
      io:format("Message received: ~p~n", [Message]),
      Pid ! F(Message),
      subscribe_loop(Pid, F)
  end.

register_subscription_name(Name, Pid, SubscriptionPid) ->
  SubscriptionName = ?SUBSCRIPTION_NAME(Name, Pid),
  case global:register_name(SubscriptionName, SubscriptionPid) of
    yes ->
      {ok, SubscriptionName};
    no ->
      {error, {already_registered, SubscriptionName}}
  end.

subscription_exists(Name, Pid) ->
  SubscriptionName = ?SUBSCRIPTION_NAME(Name, Pid),
  case global:whereis_name(SubscriptionName) of
    SubscriptionPid when is_pid(SubscriptionPid) ->
      true;
    undefined ->
      false
  end.
