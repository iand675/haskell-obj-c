{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDishwasherModeClusterModeTagStruct@.
module ObjC.Matter.MTRDishwasherModeClusterModeTagStruct
  ( MTRDishwasherModeClusterModeTagStruct
  , IsMTRDishwasherModeClusterModeTagStruct(..)
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
mfgCode :: IsMTRDishwasherModeClusterModeTagStruct mtrDishwasherModeClusterModeTagStruct => mtrDishwasherModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrDishwasherModeClusterModeTagStruct =
  sendMessage mtrDishwasherModeClusterModeTagStruct mfgCodeSelector

-- | @- setMfgCode:@
setMfgCode :: (IsMTRDishwasherModeClusterModeTagStruct mtrDishwasherModeClusterModeTagStruct, IsNSNumber value) => mtrDishwasherModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrDishwasherModeClusterModeTagStruct value =
  sendMessage mtrDishwasherModeClusterModeTagStruct setMfgCodeSelector (toNSNumber value)

-- | @- value@
value :: IsMTRDishwasherModeClusterModeTagStruct mtrDishwasherModeClusterModeTagStruct => mtrDishwasherModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrDishwasherModeClusterModeTagStruct =
  sendMessage mtrDishwasherModeClusterModeTagStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTRDishwasherModeClusterModeTagStruct mtrDishwasherModeClusterModeTagStruct, IsNSNumber value) => mtrDishwasherModeClusterModeTagStruct -> value -> IO ()
setValue mtrDishwasherModeClusterModeTagStruct value =
  sendMessage mtrDishwasherModeClusterModeTagStruct setValueSelector (toNSNumber value)

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

