# pg-bus

A `pg`-based message bus library for `purerl` (PureScript).

## Usage

A bus is set up for a name and message type. The name type can be anything, including custom types
like `newtype UserId = UserId String` or union types.

When a process subscribes it will have to provide a function that takes the bus message type and
converts it to the process message type.

Subscription and unsubscription are both idempotent, so calling them several times will not add or
remove more subscriptions. The reason for this is that subscriptions are managed by background
processes that have deterministic names, so we can check if a subscription process already exists
or not. Note that these are currently registered via `global`.

```purescript
module MyProject.ChatServer.Presence.Bus
  ( subscribe
  , unsubscribe
  , publish
  , UserEvent(..)
  ) where

import Prelude

import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Atom as Atom
import Erl.Process (Process)
import Erl.Types (Ref)
import PgBus (Bus)
import PgBus as PgBus

data UserEvent
  = UserJoined { ref :: Ref, username :: String }
  | UserLeft { ref :: Ref, username :: String }
  | UserSentMessage { ref :: Ref, username :: String, message :: String }

bus :: Bus Atom UserEvent
bus = PgBus.bus (Atom.atom "MyProject.ChatServer.Presence.Bus")

subscribe
  :: forall message
   . Process message
  -> (UserEvent -> message)
  -> Effect Unit
subscribe pid f = do
  PgBus.subscribe bus f pid

unsubscribe :: forall message. Process message -> Effect Unit
unsubscribe pid = PgBus.unsubscribe bus pid

publish :: UserEvent -> Effect Unit
publish message = do
  PgBus.publish bus message
```

Made with [`purerl`](https://github.com/purerl/purerl).
