{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureControlClusterSecureStateChangedEvent@.
module ObjC.Matter.MTRClosureControlClusterSecureStateChangedEvent
  ( MTRClosureControlClusterSecureStateChangedEvent
  , IsMTRClosureControlClusterSecureStateChangedEvent(..)
  , secureValue
  , setSecureValue
  , secureValueSelector
  , setSecureValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- secureValue@
secureValue :: IsMTRClosureControlClusterSecureStateChangedEvent mtrClosureControlClusterSecureStateChangedEvent => mtrClosureControlClusterSecureStateChangedEvent -> IO (Id NSNumber)
secureValue mtrClosureControlClusterSecureStateChangedEvent =
  sendMessage mtrClosureControlClusterSecureStateChangedEvent secureValueSelector

-- | @- setSecureValue:@
setSecureValue :: (IsMTRClosureControlClusterSecureStateChangedEvent mtrClosureControlClusterSecureStateChangedEvent, IsNSNumber value) => mtrClosureControlClusterSecureStateChangedEvent -> value -> IO ()
setSecureValue mtrClosureControlClusterSecureStateChangedEvent value =
  sendMessage mtrClosureControlClusterSecureStateChangedEvent setSecureValueSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @secureValue@
secureValueSelector :: Selector '[] (Id NSNumber)
secureValueSelector = mkSelector "secureValue"

-- | @Selector@ for @setSecureValue:@
setSecureValueSelector :: Selector '[Id NSNumber] ()
setSecureValueSelector = mkSelector "setSecureValue:"

