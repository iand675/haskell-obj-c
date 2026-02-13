{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterTariffComponentStruct@.
module ObjC.Matter.MTRCommodityTariffClusterTariffComponentStruct
  ( MTRCommodityTariffClusterTariffComponentStruct
  , IsMTRCommodityTariffClusterTariffComponentStruct(..)
  , tariffComponentID
  , setTariffComponentID
  , price
  , setPrice
  , friendlyCredit
  , setFriendlyCredit
  , auxiliaryLoad
  , setAuxiliaryLoad
  , peakPeriod
  , setPeakPeriod
  , powerThreshold
  , setPowerThreshold
  , threshold
  , setThreshold
  , label
  , setLabel
  , predicted
  , setPredicted
  , auxiliaryLoadSelector
  , friendlyCreditSelector
  , labelSelector
  , peakPeriodSelector
  , powerThresholdSelector
  , predictedSelector
  , priceSelector
  , setAuxiliaryLoadSelector
  , setFriendlyCreditSelector
  , setLabelSelector
  , setPeakPeriodSelector
  , setPowerThresholdSelector
  , setPredictedSelector
  , setPriceSelector
  , setTariffComponentIDSelector
  , setThresholdSelector
  , tariffComponentIDSelector
  , thresholdSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- tariffComponentID@
tariffComponentID :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id NSNumber)
tariffComponentID mtrCommodityTariffClusterTariffComponentStruct =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct tariffComponentIDSelector

-- | @- setTariffComponentID:@
setTariffComponentID :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsNSNumber value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setTariffComponentID mtrCommodityTariffClusterTariffComponentStruct value =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct setTariffComponentIDSelector (toNSNumber value)

-- | @- price@
price :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id MTRCommodityTariffClusterTariffPriceStruct)
price mtrCommodityTariffClusterTariffComponentStruct =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct priceSelector

-- | @- setPrice:@
setPrice :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsMTRCommodityTariffClusterTariffPriceStruct value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setPrice mtrCommodityTariffClusterTariffComponentStruct value =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct setPriceSelector (toMTRCommodityTariffClusterTariffPriceStruct value)

-- | @- friendlyCredit@
friendlyCredit :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id NSNumber)
friendlyCredit mtrCommodityTariffClusterTariffComponentStruct =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct friendlyCreditSelector

-- | @- setFriendlyCredit:@
setFriendlyCredit :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsNSNumber value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setFriendlyCredit mtrCommodityTariffClusterTariffComponentStruct value =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct setFriendlyCreditSelector (toNSNumber value)

-- | @- auxiliaryLoad@
auxiliaryLoad :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id MTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct)
auxiliaryLoad mtrCommodityTariffClusterTariffComponentStruct =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct auxiliaryLoadSelector

-- | @- setAuxiliaryLoad:@
setAuxiliaryLoad :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsMTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setAuxiliaryLoad mtrCommodityTariffClusterTariffComponentStruct value =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct setAuxiliaryLoadSelector (toMTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct value)

-- | @- peakPeriod@
peakPeriod :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id MTRCommodityTariffClusterPeakPeriodStruct)
peakPeriod mtrCommodityTariffClusterTariffComponentStruct =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct peakPeriodSelector

-- | @- setPeakPeriod:@
setPeakPeriod :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsMTRCommodityTariffClusterPeakPeriodStruct value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setPeakPeriod mtrCommodityTariffClusterTariffComponentStruct value =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct setPeakPeriodSelector (toMTRCommodityTariffClusterPeakPeriodStruct value)

-- | @- powerThreshold@
powerThreshold :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id MTRDataTypePowerThresholdStruct)
powerThreshold mtrCommodityTariffClusterTariffComponentStruct =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct powerThresholdSelector

-- | @- setPowerThreshold:@
setPowerThreshold :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsMTRDataTypePowerThresholdStruct value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setPowerThreshold mtrCommodityTariffClusterTariffComponentStruct value =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct setPowerThresholdSelector (toMTRDataTypePowerThresholdStruct value)

-- | @- threshold@
threshold :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id NSNumber)
threshold mtrCommodityTariffClusterTariffComponentStruct =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct thresholdSelector

-- | @- setThreshold:@
setThreshold :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsNSNumber value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setThreshold mtrCommodityTariffClusterTariffComponentStruct value =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct setThresholdSelector (toNSNumber value)

-- | @- label@
label :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id NSString)
label mtrCommodityTariffClusterTariffComponentStruct =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsNSString value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setLabel mtrCommodityTariffClusterTariffComponentStruct value =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct setLabelSelector (toNSString value)

-- | @- predicted@
predicted :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id NSNumber)
predicted mtrCommodityTariffClusterTariffComponentStruct =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct predictedSelector

-- | @- setPredicted:@
setPredicted :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsNSNumber value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setPredicted mtrCommodityTariffClusterTariffComponentStruct value =
  sendMessage mtrCommodityTariffClusterTariffComponentStruct setPredictedSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tariffComponentID@
tariffComponentIDSelector :: Selector '[] (Id NSNumber)
tariffComponentIDSelector = mkSelector "tariffComponentID"

-- | @Selector@ for @setTariffComponentID:@
setTariffComponentIDSelector :: Selector '[Id NSNumber] ()
setTariffComponentIDSelector = mkSelector "setTariffComponentID:"

-- | @Selector@ for @price@
priceSelector :: Selector '[] (Id MTRCommodityTariffClusterTariffPriceStruct)
priceSelector = mkSelector "price"

-- | @Selector@ for @setPrice:@
setPriceSelector :: Selector '[Id MTRCommodityTariffClusterTariffPriceStruct] ()
setPriceSelector = mkSelector "setPrice:"

-- | @Selector@ for @friendlyCredit@
friendlyCreditSelector :: Selector '[] (Id NSNumber)
friendlyCreditSelector = mkSelector "friendlyCredit"

-- | @Selector@ for @setFriendlyCredit:@
setFriendlyCreditSelector :: Selector '[Id NSNumber] ()
setFriendlyCreditSelector = mkSelector "setFriendlyCredit:"

-- | @Selector@ for @auxiliaryLoad@
auxiliaryLoadSelector :: Selector '[] (Id MTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct)
auxiliaryLoadSelector = mkSelector "auxiliaryLoad"

-- | @Selector@ for @setAuxiliaryLoad:@
setAuxiliaryLoadSelector :: Selector '[Id MTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct] ()
setAuxiliaryLoadSelector = mkSelector "setAuxiliaryLoad:"

-- | @Selector@ for @peakPeriod@
peakPeriodSelector :: Selector '[] (Id MTRCommodityTariffClusterPeakPeriodStruct)
peakPeriodSelector = mkSelector "peakPeriod"

-- | @Selector@ for @setPeakPeriod:@
setPeakPeriodSelector :: Selector '[Id MTRCommodityTariffClusterPeakPeriodStruct] ()
setPeakPeriodSelector = mkSelector "setPeakPeriod:"

-- | @Selector@ for @powerThreshold@
powerThresholdSelector :: Selector '[] (Id MTRDataTypePowerThresholdStruct)
powerThresholdSelector = mkSelector "powerThreshold"

-- | @Selector@ for @setPowerThreshold:@
setPowerThresholdSelector :: Selector '[Id MTRDataTypePowerThresholdStruct] ()
setPowerThresholdSelector = mkSelector "setPowerThreshold:"

-- | @Selector@ for @threshold@
thresholdSelector :: Selector '[] (Id NSNumber)
thresholdSelector = mkSelector "threshold"

-- | @Selector@ for @setThreshold:@
setThresholdSelector :: Selector '[Id NSNumber] ()
setThresholdSelector = mkSelector "setThreshold:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @predicted@
predictedSelector :: Selector '[] (Id NSNumber)
predictedSelector = mkSelector "predicted"

-- | @Selector@ for @setPredicted:@
setPredictedSelector :: Selector '[Id NSNumber] ()
setPredictedSelector = mkSelector "setPredicted:"

