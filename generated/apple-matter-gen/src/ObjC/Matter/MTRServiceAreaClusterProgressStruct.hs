{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRServiceAreaClusterProgressStruct@.
module ObjC.Matter.MTRServiceAreaClusterProgressStruct
  ( MTRServiceAreaClusterProgressStruct
  , IsMTRServiceAreaClusterProgressStruct(..)
  , areaID
  , setAreaID
  , status
  , setStatus
  , totalOperationalTime
  , setTotalOperationalTime
  , estimatedTime
  , setEstimatedTime
  , areaIDSelector
  , estimatedTimeSelector
  , setAreaIDSelector
  , setEstimatedTimeSelector
  , setStatusSelector
  , setTotalOperationalTimeSelector
  , statusSelector
  , totalOperationalTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- areaID@
areaID :: IsMTRServiceAreaClusterProgressStruct mtrServiceAreaClusterProgressStruct => mtrServiceAreaClusterProgressStruct -> IO (Id NSNumber)
areaID mtrServiceAreaClusterProgressStruct =
  sendMessage mtrServiceAreaClusterProgressStruct areaIDSelector

-- | @- setAreaID:@
setAreaID :: (IsMTRServiceAreaClusterProgressStruct mtrServiceAreaClusterProgressStruct, IsNSNumber value) => mtrServiceAreaClusterProgressStruct -> value -> IO ()
setAreaID mtrServiceAreaClusterProgressStruct value =
  sendMessage mtrServiceAreaClusterProgressStruct setAreaIDSelector (toNSNumber value)

-- | @- status@
status :: IsMTRServiceAreaClusterProgressStruct mtrServiceAreaClusterProgressStruct => mtrServiceAreaClusterProgressStruct -> IO (Id NSNumber)
status mtrServiceAreaClusterProgressStruct =
  sendMessage mtrServiceAreaClusterProgressStruct statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRServiceAreaClusterProgressStruct mtrServiceAreaClusterProgressStruct, IsNSNumber value) => mtrServiceAreaClusterProgressStruct -> value -> IO ()
setStatus mtrServiceAreaClusterProgressStruct value =
  sendMessage mtrServiceAreaClusterProgressStruct setStatusSelector (toNSNumber value)

-- | @- totalOperationalTime@
totalOperationalTime :: IsMTRServiceAreaClusterProgressStruct mtrServiceAreaClusterProgressStruct => mtrServiceAreaClusterProgressStruct -> IO (Id NSNumber)
totalOperationalTime mtrServiceAreaClusterProgressStruct =
  sendMessage mtrServiceAreaClusterProgressStruct totalOperationalTimeSelector

-- | @- setTotalOperationalTime:@
setTotalOperationalTime :: (IsMTRServiceAreaClusterProgressStruct mtrServiceAreaClusterProgressStruct, IsNSNumber value) => mtrServiceAreaClusterProgressStruct -> value -> IO ()
setTotalOperationalTime mtrServiceAreaClusterProgressStruct value =
  sendMessage mtrServiceAreaClusterProgressStruct setTotalOperationalTimeSelector (toNSNumber value)

-- | @- estimatedTime@
estimatedTime :: IsMTRServiceAreaClusterProgressStruct mtrServiceAreaClusterProgressStruct => mtrServiceAreaClusterProgressStruct -> IO (Id NSNumber)
estimatedTime mtrServiceAreaClusterProgressStruct =
  sendMessage mtrServiceAreaClusterProgressStruct estimatedTimeSelector

-- | @- setEstimatedTime:@
setEstimatedTime :: (IsMTRServiceAreaClusterProgressStruct mtrServiceAreaClusterProgressStruct, IsNSNumber value) => mtrServiceAreaClusterProgressStruct -> value -> IO ()
setEstimatedTime mtrServiceAreaClusterProgressStruct value =
  sendMessage mtrServiceAreaClusterProgressStruct setEstimatedTimeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @areaID@
areaIDSelector :: Selector '[] (Id NSNumber)
areaIDSelector = mkSelector "areaID"

-- | @Selector@ for @setAreaID:@
setAreaIDSelector :: Selector '[Id NSNumber] ()
setAreaIDSelector = mkSelector "setAreaID:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @totalOperationalTime@
totalOperationalTimeSelector :: Selector '[] (Id NSNumber)
totalOperationalTimeSelector = mkSelector "totalOperationalTime"

-- | @Selector@ for @setTotalOperationalTime:@
setTotalOperationalTimeSelector :: Selector '[Id NSNumber] ()
setTotalOperationalTimeSelector = mkSelector "setTotalOperationalTime:"

-- | @Selector@ for @estimatedTime@
estimatedTimeSelector :: Selector '[] (Id NSNumber)
estimatedTimeSelector = mkSelector "estimatedTime"

-- | @Selector@ for @setEstimatedTime:@
setEstimatedTimeSelector :: Selector '[Id NSNumber] ()
setEstimatedTimeSelector = mkSelector "setEstimatedTime:"

