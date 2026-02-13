{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEModeClusterModeTagStruct@.
module ObjC.Matter.MTREnergyEVSEModeClusterModeTagStruct
  ( MTREnergyEVSEModeClusterModeTagStruct
  , IsMTREnergyEVSEModeClusterModeTagStruct(..)
  , mfgCode
  , setMfgCode
  , value
  , setValue
  , mfgCodeSelector
  , setMfgCodeSelector
  , setValueSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- mfgCode@
mfgCode :: IsMTREnergyEVSEModeClusterModeTagStruct mtrEnergyEVSEModeClusterModeTagStruct => mtrEnergyEVSEModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrEnergyEVSEModeClusterModeTagStruct =
  sendMessage mtrEnergyEVSEModeClusterModeTagStruct mfgCodeSelector

-- | @- setMfgCode:@
setMfgCode :: (IsMTREnergyEVSEModeClusterModeTagStruct mtrEnergyEVSEModeClusterModeTagStruct, IsNSNumber value) => mtrEnergyEVSEModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrEnergyEVSEModeClusterModeTagStruct value =
  sendMessage mtrEnergyEVSEModeClusterModeTagStruct setMfgCodeSelector (toNSNumber value)

-- | @- value@
value :: IsMTREnergyEVSEModeClusterModeTagStruct mtrEnergyEVSEModeClusterModeTagStruct => mtrEnergyEVSEModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrEnergyEVSEModeClusterModeTagStruct =
  sendMessage mtrEnergyEVSEModeClusterModeTagStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTREnergyEVSEModeClusterModeTagStruct mtrEnergyEVSEModeClusterModeTagStruct, IsNSNumber value) => mtrEnergyEVSEModeClusterModeTagStruct -> value -> IO ()
setValue mtrEnergyEVSEModeClusterModeTagStruct value =
  sendMessage mtrEnergyEVSEModeClusterModeTagStruct setValueSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mfgCode@
mfgCodeSelector :: Selector '[] (Id NSNumber)
mfgCodeSelector = mkSelector "mfgCode"

-- | @Selector@ for @setMfgCode:@
setMfgCodeSelector :: Selector '[Id NSNumber] ()
setMfgCodeSelector = mkSelector "setMfgCode:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSNumber)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSNumber] ()
setValueSelector = mkSelector "setValue:"

