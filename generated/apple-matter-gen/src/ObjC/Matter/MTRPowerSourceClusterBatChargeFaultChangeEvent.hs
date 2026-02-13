{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPowerSourceClusterBatChargeFaultChangeEvent@.
module ObjC.Matter.MTRPowerSourceClusterBatChargeFaultChangeEvent
  ( MTRPowerSourceClusterBatChargeFaultChangeEvent
  , IsMTRPowerSourceClusterBatChargeFaultChangeEvent(..)
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
current :: IsMTRPowerSourceClusterBatChargeFaultChangeEvent mtrPowerSourceClusterBatChargeFaultChangeEvent => mtrPowerSourceClusterBatChargeFaultChangeEvent -> IO (Id NSArray)
current mtrPowerSourceClusterBatChargeFaultChangeEvent =
  sendMessage mtrPowerSourceClusterBatChargeFaultChangeEvent currentSelector

-- | @- setCurrent:@
setCurrent :: (IsMTRPowerSourceClusterBatChargeFaultChangeEvent mtrPowerSourceClusterBatChargeFaultChangeEvent, IsNSArray value) => mtrPowerSourceClusterBatChargeFaultChangeEvent -> value -> IO ()
setCurrent mtrPowerSourceClusterBatChargeFaultChangeEvent value =
  sendMessage mtrPowerSourceClusterBatChargeFaultChangeEvent setCurrentSelector (toNSArray value)

-- | @- previous@
previous :: IsMTRPowerSourceClusterBatChargeFaultChangeEvent mtrPowerSourceClusterBatChargeFaultChangeEvent => mtrPowerSourceClusterBatChargeFaultChangeEvent -> IO (Id NSArray)
previous mtrPowerSourceClusterBatChargeFaultChangeEvent =
  sendMessage mtrPowerSourceClusterBatChargeFaultChangeEvent previousSelector

-- | @- setPrevious:@
setPrevious :: (IsMTRPowerSourceClusterBatChargeFaultChangeEvent mtrPowerSourceClusterBatChargeFaultChangeEvent, IsNSArray value) => mtrPowerSourceClusterBatChargeFaultChangeEvent -> value -> IO ()
setPrevious mtrPowerSourceClusterBatChargeFaultChangeEvent value =
  sendMessage mtrPowerSourceClusterBatChargeFaultChangeEvent setPreviousSelector (toNSArray value)

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

