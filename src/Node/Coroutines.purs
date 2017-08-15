module Node.Coroutines
  ( readable
  , writable
  ) where

import Prelude

import Control.Coroutine (Consumer, Producer, await)
import Control.Coroutine.Aff (produce')
import Control.Monad.Aff (makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Node.Encoding (Encoding)
import Node.Stream (Writable, Readable, onClose, onDataString, writeString)

-- | A `Producer` which produces `String`s by reading them from
-- | some `Readable` stream.
readable
  :: forall aff r p
   . MonadAff ( avar :: AVAR
              , console :: CONSOLE
              , exception :: EXCEPTION
              | r
              ) aff
  => Encoding
  -> Readable p ( avar :: AVAR
                , console :: CONSOLE
                , exception :: EXCEPTION
                | r
                )
  -> Producer String aff Unit
readable enc s = produce' \k -> do
  onDataString s enc (k <<< Left)
  onClose s (k (Right unit))

-- | A `Consumer` which consumes `String`s by sending them to
-- | a `Writable` stream.
writable
  :: forall aff r p a
   . MonadAff ( console :: CONSOLE
              , exception :: EXCEPTION
              | r
              ) aff
  => Encoding
  -> Writable p  ( console :: CONSOLE
                , exception :: EXCEPTION
                | r
                )
  -> Consumer String aff a
writable enc w = forever do
  s <- await
  lift (liftAff (makeAff \_ k ->
    void (writeString w enc s (k unit))))
