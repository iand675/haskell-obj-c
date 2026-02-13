{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCRunModeClusterModeTagStruct@.
module ObjC.Matter.MTRRVCRunModeClusterModeTagStruct
  ( MTRRVCRunModeClusterModeTagStruct
  , IsMTRRVCRunModeClusterModeTagStruct(..)
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
mfgCode :: IsMTRRVCRunModeClusterModeTagStruct mtrrvcRunModeClusterModeTagStruct => mtrrvcRunModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrrvcRunModeClusterModeTagStruct =
  sendMessage mtrrvcRunModeClusterModeTagStruct mfgCodeSelector

-- | @- setMfgCode:@
setMfgCode :: (IsMTRRVCRunModeClusterModeTagStruct mtrrvcRunModeClusterModeTagStruct, IsNSNumber value) => mtrrvcRunModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrrvcRunModeClusterModeTagStruct value =
  sendMessage mtrrvcRunModeClusterModeTagStruct setMfgCodeSelector (toNSNumber value)

-- | @- value@
value :: IsMTRRVCRunModeClusterModeTagStruct mtrrvcRunModeClusterModeTagStruct => mtrrvcRunModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrrvcRunModeClusterModeTagStruct =
  sendMessage mtrrvcRunModeClusterModeTagStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTRRVCRunModeClusterModeTagStruct mtrrvcRunModeClusterModeTagStruct, IsNSNumber value) => mtrrvcRunModeClusterModeTagStruct -> value -> IO ()
setValue mtrrvcRunModeClusterModeTagStruct value =
  sendMessage mtrrvcRunModeClusterModeTagStruct setValueSelector (toNSNumber value)

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

