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
  , initSelector
  , newSelector
  , didRecoverSelector
  , errorSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVMetricErrorEvent avMetricErrorEvent => avMetricErrorEvent -> IO (Id AVMetricErrorEvent)
init_ avMetricErrorEvent  =
  sendMsg avMetricErrorEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMetricErrorEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricErrorEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns whether the error was recoverable.
--
-- ObjC selector: @- didRecover@
didRecover :: IsAVMetricErrorEvent avMetricErrorEvent => avMetricErrorEvent -> IO Bool
didRecover avMetricErrorEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMetricErrorEvent (mkSelector "didRecover") retCULong []

-- | Returns the error encountered.
--
-- ObjC selector: @- error@
error_ :: IsAVMetricErrorEvent avMetricErrorEvent => avMetricErrorEvent -> IO (Id NSError)
error_ avMetricErrorEvent  =
  sendMsg avMetricErrorEvent (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @didRecover@
didRecoverSelector :: Selector
didRecoverSelector = mkSelector "didRecover"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

