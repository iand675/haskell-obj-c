{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMicrowaveOvenModeClusterModeTagStruct@.
module ObjC.Matter.MTRMicrowaveOvenModeClusterModeTagStruct
  ( MTRMicrowaveOvenModeClusterModeTagStruct
  , IsMTRMicrowaveOvenModeClusterModeTagStruct(..)
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
mfgCode :: IsMTRMicrowaveOvenModeClusterModeTagStruct mtrMicrowaveOvenModeClusterModeTagStruct => mtrMicrowaveOvenModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrMicrowaveOvenModeClusterModeTagStruct =
  sendMessage mtrMicrowaveOvenModeClusterModeTagStruct mfgCodeSelector

-- | @- setMfgCode:@
setMfgCode :: (IsMTRMicrowaveOvenModeClusterModeTagStruct mtrMicrowaveOvenModeClusterModeTagStruct, IsNSNumber value) => mtrMicrowaveOvenModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrMicrowaveOvenModeClusterModeTagStruct value =
  sendMessage mtrMicrowaveOvenModeClusterModeTagStruct setMfgCodeSelector (toNSNumber value)

-- | @- value@
value :: IsMTRMicrowaveOvenModeClusterModeTagStruct mtrMicrowaveOvenModeClusterModeTagStruct => mtrMicrowaveOvenModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrMicrowaveOvenModeClusterModeTagStruct =
  sendMessage mtrMicrowaveOvenModeClusterModeTagStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTRMicrowaveOvenModeClusterModeTagStruct mtrMicrowaveOvenModeClusterModeTagStruct, IsNSNumber value) => mtrMicrowaveOvenModeClusterModeTagStruct -> value -> IO ()
setValue mtrMicrowaveOvenModeClusterModeTagStruct value =
  sendMessage mtrMicrowaveOvenModeClusterModeTagStruct setValueSelector (toNSNumber value)

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

