module PgBus
  ( Bus
  , bus
  , subscribe
  , unsubscribe
  , publish
  ) where

import Prelude

import Effect (Effect)
import Erl.Process (Process)
import Unsafe.Coerce as Coerce

foreign import data Bus :: forall name message. name -> message -> Type

bus :: forall name message. name -> Bus name message
bus name = Coerce.unsafeCoerce name

foreign import subscribe
  :: forall name busMessage processMessage
   . Bus name busMessage
  -> (busMessage -> processMessage)
  -> Process processMessage
  -> Effect Unit

foreign import unsubscribe
  :: forall name busMessage processMessage
   . Bus name busMessage
  -> Process processMessage
  -> Effect Unit

foreign import publish :: forall name message. Bus name message -> message -> Effect Unit
