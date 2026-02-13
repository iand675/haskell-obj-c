{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPowerSourceClusterBatChargeFaultChangeType@.
module ObjC.Matter.MTRPowerSourceClusterBatChargeFaultChangeType
  ( MTRPowerSourceClusterBatChargeFaultChangeType
  , IsMTRPowerSourceClusterBatChargeFaultChangeType(..)
  , current
  , setCurrent
  , previous
  , setPrevious
  , currentSelector
  , previousSelector
  , setCurrentSelector
  , setPreviousSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- current@
current :: IsMTRPowerSourceClusterBatChargeFaultChangeType mtrPowerSourceClusterBatChargeFaultChangeType => mtrPowerSourceClusterBatChargeFaultChangeType -> IO (Id NSArray)
current mtrPowerSourceClusterBatChargeFaultChangeType =
  sendMessage mtrPowerSourceClusterBatChargeFaultChangeType currentSelector

-- | @- setCurrent:@
setCurrent :: (IsMTRPowerSourceClusterBatChargeFaultChangeType mtrPowerSourceClusterBatChargeFaultChangeType, IsNSArray value) => mtrPowerSourceClusterBatChargeFaultChangeType -> value -> IO ()
setCurrent mtrPowerSourceClusterBatChargeFaultChangeType value =
  sendMessage mtrPowerSourceClusterBatChargeFaultChangeType setCurrentSelector (toNSArray value)

-- | @- previous@
previous :: IsMTRPowerSourceClusterBatChargeFaultChangeType mtrPowerSourceClusterBatChargeFaultChangeType => mtrPowerSourceClusterBatChargeFaultChangeType -> IO (Id NSArray)
previous mtrPowerSourceClusterBatChargeFaultChangeType =
  sendMessage mtrPowerSourceClusterBatChargeFaultChangeType previousSelector

-- | @- setPrevious:@
setPrevious :: (IsMTRPowerSourceClusterBatChargeFaultChangeType mtrPowerSourceClusterBatChargeFaultChangeType, IsNSArray value) => mtrPowerSourceClusterBatChargeFaultChangeType -> value -> IO ()
setPrevious mtrPowerSourceClusterBatChargeFaultChangeType value =
  sendMessage mtrPowerSourceClusterBatChargeFaultChangeType setPreviousSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @current@
currentSelector :: Selector '[] (Id NSArray)
currentSelector = mkSelector "current"

-- | @Selector@ for @setCurrent:@
setCurrentSelector :: Selector '[Id NSArray] ()
setCurrentSelector = mkSelector "setCurrent:"

-- | @Selector@ for @previous@
previousSelector :: Selector '[] (Id NSArray)
previousSelector = mkSelector "previous"

-- | @Selector@ for @setPrevious:@
setPreviousSelector :: Selector '[Id NSArray] ()
setPreviousSelector = mkSelector "setPrevious:"

