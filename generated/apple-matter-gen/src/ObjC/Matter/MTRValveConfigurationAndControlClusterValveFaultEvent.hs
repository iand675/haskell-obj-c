{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRValveConfigurationAndControlClusterValveFaultEvent@.
module ObjC.Matter.MTRValveConfigurationAndControlClusterValveFaultEvent
  ( MTRValveConfigurationAndControlClusterValveFaultEvent
  , IsMTRValveConfigurationAndControlClusterValveFaultEvent(..)
  , valveFault
  , setValveFault
  , setValveFaultSelector
  , valveFaultSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- valveFault@
valveFault :: IsMTRValveConfigurationAndControlClusterValveFaultEvent mtrValveConfigurationAndControlClusterValveFaultEvent => mtrValveConfigurationAndControlClusterValveFaultEvent -> IO (Id NSNumber)
valveFault mtrValveConfigurationAndControlClusterValveFaultEvent =
  sendMessage mtrValveConfigurationAndControlClusterValveFaultEvent valveFaultSelector

-- | @- setValveFault:@
setValveFault :: (IsMTRValveConfigurationAndControlClusterValveFaultEvent mtrValveConfigurationAndControlClusterValveFaultEvent, IsNSNumber value) => mtrValveConfigurationAndControlClusterValveFaultEvent -> value -> IO ()
setValveFault mtrValveConfigurationAndControlClusterValveFaultEvent value =
  sendMessage mtrValveConfigurationAndControlClusterValveFaultEvent setValveFaultSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valveFault@
valveFaultSelector :: Selector '[] (Id NSNumber)
valveFaultSelector = mkSelector "valveFault"

-- | @Selector@ for @setValveFault:@
setValveFaultSelector :: Selector '[Id NSNumber] ()
setValveFaultSelector = mkSelector "setValveFault:"

