{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event when an error occurred.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricErrorEvent@.
module ObjC.AVFoundation.AVMetricErrorEvent
  ( AVMetricErrorEvent
  , IsAVMetricErrorEvent(..)
  , init_
  , new
  , didRecover
  , error_
  , didRecoverSelector
  , errorSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVMetricErrorEvent avMetricErrorEvent => avMetricErrorEvent -> IO (Id AVMetricErrorEvent)
init_ avMetricErrorEvent =
  sendOwnedMessage avMetricErrorEvent initSelector

-- | @+ new@
new :: IO (Id AVMetricErrorEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricErrorEvent"
    sendOwnedClassMessage cls' newSelector

-- | Returns whether the error was recoverable.
--
-- ObjC selector: @- didRecover@
didRecover :: IsAVMetricErrorEvent avMetricErrorEvent => avMetricErrorEvent -> IO Bool
didRecover avMetricErrorEvent =
  sendMessage avMetricErrorEvent didRecoverSelector

-- | Returns the error encountered.
--
-- ObjC selector: @- error@
error_ :: IsAVMetricErrorEvent avMetricErrorEvent => avMetricErrorEvent -> IO (Id NSError)
error_ avMetricErrorEvent =
  sendMessage avMetricErrorEvent errorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricErrorEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricErrorEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @didRecover@
didRecoverSelector :: Selector '[] Bool
didRecoverSelector = mkSelector "didRecover"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

