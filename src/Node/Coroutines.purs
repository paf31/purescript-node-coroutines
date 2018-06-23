module Node.Coroutines
  ( readable
  , writable
  ) where

import Prelude

import Effect.Aff (makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Control.Coroutine (Consumer, Producer, await)
import Control.Coroutine.Aff (close, emit, produce')
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Node.Encoding (Encoding)
import Node.Stream (Writable, Readable, onClose, onDataString, writeString)

-- | A `Producer` which produces `String`s by reading them from
-- | some `Readable` stream.
readable
  :: forall aff p
   . MonadAff aff
  => Encoding
  -> Readable p 
  -> Producer String aff Unit
readable enc s = produce' \k -> do
  onDataString s enc (emit k)
  onClose s (close k unit)

-- | A `Consumer` which consumes `String`s by sending them to
-- | a `Writable` stream.
writable
  :: forall aff p a
   . MonadAff aff
  => Encoding
  -> Writable p
  -> Consumer String aff a
writable enc w = forever do
  s <- await
  lift (liftAff (makeAff \k -> do
    void (writeString w enc s (k (Right unit)))
    pure nonCanceler))
