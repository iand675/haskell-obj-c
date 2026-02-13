{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypePowerThresholdStruct@.
module ObjC.Matter.MTRDataTypePowerThresholdStruct
  ( MTRDataTypePowerThresholdStruct
  , IsMTRDataTypePowerThresholdStruct(..)
  , powerThreshold
  , setPowerThreshold
  , apparentPowerThreshold
  , setApparentPowerThreshold
  , powerThresholdSource
  , setPowerThresholdSource
  , apparentPowerThresholdSelector
  , powerThresholdSelector
  , powerThresholdSourceSelector
  , setApparentPowerThresholdSelector
  , setPowerThresholdSelector
  , setPowerThresholdSourceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- powerThreshold@
powerThreshold :: IsMTRDataTypePowerThresholdStruct mtrDataTypePowerThresholdStruct => mtrDataTypePowerThresholdStruct -> IO (Id NSNumber)
powerThreshold mtrDataTypePowerThresholdStruct =
  sendMessage mtrDataTypePowerThresholdStruct powerThresholdSelector

-- | @- setPowerThreshold:@
setPowerThreshold :: (IsMTRDataTypePowerThresholdStruct mtrDataTypePowerThresholdStruct, IsNSNumber value) => mtrDataTypePowerThresholdStruct -> value -> IO ()
setPowerThreshold mtrDataTypePowerThresholdStruct value =
  sendMessage mtrDataTypePowerThresholdStruct setPowerThresholdSelector (toNSNumber value)

-- | @- apparentPowerThreshold@
apparentPowerThreshold :: IsMTRDataTypePowerThresholdStruct mtrDataTypePowerThresholdStruct => mtrDataTypePowerThresholdStruct -> IO (Id NSNumber)
apparentPowerThreshold mtrDataTypePowerThresholdStruct =
  sendMessage mtrDataTypePowerThresholdStruct apparentPowerThresholdSelector

-- | @- setApparentPowerThreshold:@
setApparentPowerThreshold :: (IsMTRDataTypePowerThresholdStruct mtrDataTypePowerThresholdStruct, IsNSNumber value) => mtrDataTypePowerThresholdStruct -> value -> IO ()
setApparentPowerThreshold mtrDataTypePowerThresholdStruct value =
  sendMessage mtrDataTypePowerThresholdStruct setApparentPowerThresholdSelector (toNSNumber value)

-- | @- powerThresholdSource@
powerThresholdSource :: IsMTRDataTypePowerThresholdStruct mtrDataTypePowerThresholdStruct => mtrDataTypePowerThresholdStruct -> IO (Id NSNumber)
powerThresholdSource mtrDataTypePowerThresholdStruct =
  sendMessage mtrDataTypePowerThresholdStruct powerThresholdSourceSelector

-- | @- setPowerThresholdSource:@
setPowerThresholdSource :: (IsMTRDataTypePowerThresholdStruct mtrDataTypePowerThresholdStruct, IsNSNumber value) => mtrDataTypePowerThresholdStruct -> value -> IO ()
setPowerThresholdSource mtrDataTypePowerThresholdStruct value =
  sendMessage mtrDataTypePowerThresholdStruct setPowerThresholdSourceSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @powerThreshold@
powerThresholdSelector :: Selector '[] (Id NSNumber)
powerThresholdSelector = mkSelector "powerThreshold"

-- | @Selector@ for @setPowerThreshold:@
setPowerThresholdSelector :: Selector '[Id NSNumber] ()
setPowerThresholdSelector = mkSelector "setPowerThreshold:"

-- | @Selector@ for @apparentPowerThreshold@
apparentPowerThresholdSelector :: Selector '[] (Id NSNumber)
apparentPowerThresholdSelector = mkSelector "apparentPowerThreshold"

-- | @Selector@ for @setApparentPowerThreshold:@
setApparentPowerThresholdSelector :: Selector '[Id NSNumber] ()
setApparentPowerThresholdSelector = mkSelector "setApparentPowerThreshold:"

-- | @Selector@ for @powerThresholdSource@
powerThresholdSourceSelector :: Selector '[] (Id NSNumber)
powerThresholdSourceSelector = mkSelector "powerThresholdSource"

-- | @Selector@ for @setPowerThresholdSource:@
setPowerThresholdSourceSelector :: Selector '[Id NSNumber] ()
setPowerThresholdSourceSelector = mkSelector "setPowerThresholdSource:"

