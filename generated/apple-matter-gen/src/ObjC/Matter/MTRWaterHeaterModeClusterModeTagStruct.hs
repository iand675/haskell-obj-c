{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWaterHeaterModeClusterModeTagStruct@.
module ObjC.Matter.MTRWaterHeaterModeClusterModeTagStruct
  ( MTRWaterHeaterModeClusterModeTagStruct
  , IsMTRWaterHeaterModeClusterModeTagStruct(..)
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
mfgCode :: IsMTRWaterHeaterModeClusterModeTagStruct mtrWaterHeaterModeClusterModeTagStruct => mtrWaterHeaterModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrWaterHeaterModeClusterModeTagStruct =
  sendMessage mtrWaterHeaterModeClusterModeTagStruct mfgCodeSelector

-- | @- setMfgCode:@
setMfgCode :: (IsMTRWaterHeaterModeClusterModeTagStruct mtrWaterHeaterModeClusterModeTagStruct, IsNSNumber value) => mtrWaterHeaterModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrWaterHeaterModeClusterModeTagStruct value =
  sendMessage mtrWaterHeaterModeClusterModeTagStruct setMfgCodeSelector (toNSNumber value)

-- | @- value@
value :: IsMTRWaterHeaterModeClusterModeTagStruct mtrWaterHeaterModeClusterModeTagStruct => mtrWaterHeaterModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrWaterHeaterModeClusterModeTagStruct =
  sendMessage mtrWaterHeaterModeClusterModeTagStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTRWaterHeaterModeClusterModeTagStruct mtrWaterHeaterModeClusterModeTagStruct, IsNSNumber value) => mtrWaterHeaterModeClusterModeTagStruct -> value -> IO ()
setValue mtrWaterHeaterModeClusterModeTagStruct value =
  sendMessage mtrWaterHeaterModeClusterModeTagStruct setValueSelector (toNSNumber value)

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

