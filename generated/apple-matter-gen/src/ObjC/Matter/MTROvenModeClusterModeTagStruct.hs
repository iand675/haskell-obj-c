{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROvenModeClusterModeTagStruct@.
module ObjC.Matter.MTROvenModeClusterModeTagStruct
  ( MTROvenModeClusterModeTagStruct
  , IsMTROvenModeClusterModeTagStruct(..)
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
mfgCode :: IsMTROvenModeClusterModeTagStruct mtrOvenModeClusterModeTagStruct => mtrOvenModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrOvenModeClusterModeTagStruct =
  sendMessage mtrOvenModeClusterModeTagStruct mfgCodeSelector

-- | @- setMfgCode:@
setMfgCode :: (IsMTROvenModeClusterModeTagStruct mtrOvenModeClusterModeTagStruct, IsNSNumber value) => mtrOvenModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrOvenModeClusterModeTagStruct value =
  sendMessage mtrOvenModeClusterModeTagStruct setMfgCodeSelector (toNSNumber value)

-- | @- value@
value :: IsMTROvenModeClusterModeTagStruct mtrOvenModeClusterModeTagStruct => mtrOvenModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrOvenModeClusterModeTagStruct =
  sendMessage mtrOvenModeClusterModeTagStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTROvenModeClusterModeTagStruct mtrOvenModeClusterModeTagStruct, IsNSNumber value) => mtrOvenModeClusterModeTagStruct -> value -> IO ()
setValue mtrOvenModeClusterModeTagStruct value =
  sendMessage mtrOvenModeClusterModeTagStruct setValueSelector (toNSNumber value)

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

