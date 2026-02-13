{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCCleanModeClusterModeTagStruct@.
module ObjC.Matter.MTRRVCCleanModeClusterModeTagStruct
  ( MTRRVCCleanModeClusterModeTagStruct
  , IsMTRRVCCleanModeClusterModeTagStruct(..)
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
mfgCode :: IsMTRRVCCleanModeClusterModeTagStruct mtrrvcCleanModeClusterModeTagStruct => mtrrvcCleanModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrrvcCleanModeClusterModeTagStruct =
  sendMessage mtrrvcCleanModeClusterModeTagStruct mfgCodeSelector

-- | @- setMfgCode:@
setMfgCode :: (IsMTRRVCCleanModeClusterModeTagStruct mtrrvcCleanModeClusterModeTagStruct, IsNSNumber value) => mtrrvcCleanModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrrvcCleanModeClusterModeTagStruct value =
  sendMessage mtrrvcCleanModeClusterModeTagStruct setMfgCodeSelector (toNSNumber value)

-- | @- value@
value :: IsMTRRVCCleanModeClusterModeTagStruct mtrrvcCleanModeClusterModeTagStruct => mtrrvcCleanModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrrvcCleanModeClusterModeTagStruct =
  sendMessage mtrrvcCleanModeClusterModeTagStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTRRVCCleanModeClusterModeTagStruct mtrrvcCleanModeClusterModeTagStruct, IsNSNumber value) => mtrrvcCleanModeClusterModeTagStruct -> value -> IO ()
setValue mtrrvcCleanModeClusterModeTagStruct value =
  sendMessage mtrrvcCleanModeClusterModeTagStruct setValueSelector (toNSNumber value)

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

