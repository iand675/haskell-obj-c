{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalGridConditionsClusterElectricalGridConditionsStruct@.
module ObjC.Matter.MTRElectricalGridConditionsClusterElectricalGridConditionsStruct
  ( MTRElectricalGridConditionsClusterElectricalGridConditionsStruct
  , IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct(..)
  , periodStart
  , setPeriodStart
  , periodEnd
  , setPeriodEnd
  , gridCarbonIntensity
  , setGridCarbonIntensity
  , gridCarbonLevel
  , setGridCarbonLevel
  , localCarbonIntensity
  , setLocalCarbonIntensity
  , localCarbonLevel
  , setLocalCarbonLevel
  , gridCarbonIntensitySelector
  , gridCarbonLevelSelector
  , localCarbonIntensitySelector
  , localCarbonLevelSelector
  , periodEndSelector
  , periodStartSelector
  , setGridCarbonIntensitySelector
  , setGridCarbonLevelSelector
  , setLocalCarbonIntensitySelector
  , setLocalCarbonLevelSelector
  , setPeriodEndSelector
  , setPeriodStartSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- periodStart@
periodStart :: IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> IO (Id NSNumber)
periodStart mtrElectricalGridConditionsClusterElectricalGridConditionsStruct =
  sendMessage mtrElectricalGridConditionsClusterElectricalGridConditionsStruct periodStartSelector

-- | @- setPeriodStart:@
setPeriodStart :: (IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct, IsNSNumber value) => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> value -> IO ()
setPeriodStart mtrElectricalGridConditionsClusterElectricalGridConditionsStruct value =
  sendMessage mtrElectricalGridConditionsClusterElectricalGridConditionsStruct setPeriodStartSelector (toNSNumber value)

-- | @- periodEnd@
periodEnd :: IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> IO (Id NSNumber)
periodEnd mtrElectricalGridConditionsClusterElectricalGridConditionsStruct =
  sendMessage mtrElectricalGridConditionsClusterElectricalGridConditionsStruct periodEndSelector

-- | @- setPeriodEnd:@
setPeriodEnd :: (IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct, IsNSNumber value) => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> value -> IO ()
setPeriodEnd mtrElectricalGridConditionsClusterElectricalGridConditionsStruct value =
  sendMessage mtrElectricalGridConditionsClusterElectricalGridConditionsStruct setPeriodEndSelector (toNSNumber value)

-- | @- gridCarbonIntensity@
gridCarbonIntensity :: IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> IO (Id NSNumber)
gridCarbonIntensity mtrElectricalGridConditionsClusterElectricalGridConditionsStruct =
  sendMessage mtrElectricalGridConditionsClusterElectricalGridConditionsStruct gridCarbonIntensitySelector

-- | @- setGridCarbonIntensity:@
setGridCarbonIntensity :: (IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct, IsNSNumber value) => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> value -> IO ()
setGridCarbonIntensity mtrElectricalGridConditionsClusterElectricalGridConditionsStruct value =
  sendMessage mtrElectricalGridConditionsClusterElectricalGridConditionsStruct setGridCarbonIntensitySelector (toNSNumber value)

-- | @- gridCarbonLevel@
gridCarbonLevel :: IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> IO (Id NSNumber)
gridCarbonLevel mtrElectricalGridConditionsClusterElectricalGridConditionsStruct =
  sendMessage mtrElectricalGridConditionsClusterElectricalGridConditionsStruct gridCarbonLevelSelector

-- | @- setGridCarbonLevel:@
setGridCarbonLevel :: (IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct, IsNSNumber value) => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> value -> IO ()
setGridCarbonLevel mtrElectricalGridConditionsClusterElectricalGridConditionsStruct value =
  sendMessage mtrElectricalGridConditionsClusterElectricalGridConditionsStruct setGridCarbonLevelSelector (toNSNumber value)

-- | @- localCarbonIntensity@
localCarbonIntensity :: IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> IO (Id NSNumber)
localCarbonIntensity mtrElectricalGridConditionsClusterElectricalGridConditionsStruct =
  sendMessage mtrElectricalGridConditionsClusterElectricalGridConditionsStruct localCarbonIntensitySelector

-- | @- setLocalCarbonIntensity:@
setLocalCarbonIntensity :: (IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct, IsNSNumber value) => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> value -> IO ()
setLocalCarbonIntensity mtrElectricalGridConditionsClusterElectricalGridConditionsStruct value =
  sendMessage mtrElectricalGridConditionsClusterElectricalGridConditionsStruct setLocalCarbonIntensitySelector (toNSNumber value)

-- | @- localCarbonLevel@
localCarbonLevel :: IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> IO (Id NSNumber)
localCarbonLevel mtrElectricalGridConditionsClusterElectricalGridConditionsStruct =
  sendMessage mtrElectricalGridConditionsClusterElectricalGridConditionsStruct localCarbonLevelSelector

-- | @- setLocalCarbonLevel:@
setLocalCarbonLevel :: (IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct, IsNSNumber value) => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> value -> IO ()
setLocalCarbonLevel mtrElectricalGridConditionsClusterElectricalGridConditionsStruct value =
  sendMessage mtrElectricalGridConditionsClusterElectricalGridConditionsStruct setLocalCarbonLevelSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @periodStart@
periodStartSelector :: Selector '[] (Id NSNumber)
periodStartSelector = mkSelector "periodStart"

-- | @Selector@ for @setPeriodStart:@
setPeriodStartSelector :: Selector '[Id NSNumber] ()
setPeriodStartSelector = mkSelector "setPeriodStart:"

-- | @Selector@ for @periodEnd@
periodEndSelector :: Selector '[] (Id NSNumber)
periodEndSelector = mkSelector "periodEnd"

-- | @Selector@ for @setPeriodEnd:@
setPeriodEndSelector :: Selector '[Id NSNumber] ()
setPeriodEndSelector = mkSelector "setPeriodEnd:"

-- | @Selector@ for @gridCarbonIntensity@
gridCarbonIntensitySelector :: Selector '[] (Id NSNumber)
gridCarbonIntensitySelector = mkSelector "gridCarbonIntensity"

-- | @Selector@ for @setGridCarbonIntensity:@
setGridCarbonIntensitySelector :: Selector '[Id NSNumber] ()
setGridCarbonIntensitySelector = mkSelector "setGridCarbonIntensity:"

-- | @Selector@ for @gridCarbonLevel@
gridCarbonLevelSelector :: Selector '[] (Id NSNumber)
gridCarbonLevelSelector = mkSelector "gridCarbonLevel"

-- | @Selector@ for @setGridCarbonLevel:@
setGridCarbonLevelSelector :: Selector '[Id NSNumber] ()
setGridCarbonLevelSelector = mkSelector "setGridCarbonLevel:"

-- | @Selector@ for @localCarbonIntensity@
localCarbonIntensitySelector :: Selector '[] (Id NSNumber)
localCarbonIntensitySelector = mkSelector "localCarbonIntensity"

-- | @Selector@ for @setLocalCarbonIntensity:@
setLocalCarbonIntensitySelector :: Selector '[Id NSNumber] ()
setLocalCarbonIntensitySelector = mkSelector "setLocalCarbonIntensity:"

-- | @Selector@ for @localCarbonLevel@
localCarbonLevelSelector :: Selector '[] (Id NSNumber)
localCarbonLevelSelector = mkSelector "localCarbonLevel"

-- | @Selector@ for @setLocalCarbonLevel:@
setLocalCarbonLevelSelector :: Selector '[Id NSNumber] ()
setLocalCarbonLevelSelector = mkSelector "setLocalCarbonLevel:"

