{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROccupancySensingClusterHoldTimeLimitsStruct@.
module ObjC.Matter.MTROccupancySensingClusterHoldTimeLimitsStruct
  ( MTROccupancySensingClusterHoldTimeLimitsStruct
  , IsMTROccupancySensingClusterHoldTimeLimitsStruct(..)
  , holdTimeMin
  , setHoldTimeMin
  , holdTimeMax
  , setHoldTimeMax
  , holdTimeDefault
  , setHoldTimeDefault
  , holdTimeDefaultSelector
  , holdTimeMaxSelector
  , holdTimeMinSelector
  , setHoldTimeDefaultSelector
  , setHoldTimeMaxSelector
  , setHoldTimeMinSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- holdTimeMin@
holdTimeMin :: IsMTROccupancySensingClusterHoldTimeLimitsStruct mtrOccupancySensingClusterHoldTimeLimitsStruct => mtrOccupancySensingClusterHoldTimeLimitsStruct -> IO (Id NSNumber)
holdTimeMin mtrOccupancySensingClusterHoldTimeLimitsStruct =
  sendMessage mtrOccupancySensingClusterHoldTimeLimitsStruct holdTimeMinSelector

-- | @- setHoldTimeMin:@
setHoldTimeMin :: (IsMTROccupancySensingClusterHoldTimeLimitsStruct mtrOccupancySensingClusterHoldTimeLimitsStruct, IsNSNumber value) => mtrOccupancySensingClusterHoldTimeLimitsStruct -> value -> IO ()
setHoldTimeMin mtrOccupancySensingClusterHoldTimeLimitsStruct value =
  sendMessage mtrOccupancySensingClusterHoldTimeLimitsStruct setHoldTimeMinSelector (toNSNumber value)

-- | @- holdTimeMax@
holdTimeMax :: IsMTROccupancySensingClusterHoldTimeLimitsStruct mtrOccupancySensingClusterHoldTimeLimitsStruct => mtrOccupancySensingClusterHoldTimeLimitsStruct -> IO (Id NSNumber)
holdTimeMax mtrOccupancySensingClusterHoldTimeLimitsStruct =
  sendMessage mtrOccupancySensingClusterHoldTimeLimitsStruct holdTimeMaxSelector

-- | @- setHoldTimeMax:@
setHoldTimeMax :: (IsMTROccupancySensingClusterHoldTimeLimitsStruct mtrOccupancySensingClusterHoldTimeLimitsStruct, IsNSNumber value) => mtrOccupancySensingClusterHoldTimeLimitsStruct -> value -> IO ()
setHoldTimeMax mtrOccupancySensingClusterHoldTimeLimitsStruct value =
  sendMessage mtrOccupancySensingClusterHoldTimeLimitsStruct setHoldTimeMaxSelector (toNSNumber value)

-- | @- holdTimeDefault@
holdTimeDefault :: IsMTROccupancySensingClusterHoldTimeLimitsStruct mtrOccupancySensingClusterHoldTimeLimitsStruct => mtrOccupancySensingClusterHoldTimeLimitsStruct -> IO (Id NSNumber)
holdTimeDefault mtrOccupancySensingClusterHoldTimeLimitsStruct =
  sendMessage mtrOccupancySensingClusterHoldTimeLimitsStruct holdTimeDefaultSelector

-- | @- setHoldTimeDefault:@
setHoldTimeDefault :: (IsMTROccupancySensingClusterHoldTimeLimitsStruct mtrOccupancySensingClusterHoldTimeLimitsStruct, IsNSNumber value) => mtrOccupancySensingClusterHoldTimeLimitsStruct -> value -> IO ()
setHoldTimeDefault mtrOccupancySensingClusterHoldTimeLimitsStruct value =
  sendMessage mtrOccupancySensingClusterHoldTimeLimitsStruct setHoldTimeDefaultSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @holdTimeMin@
holdTimeMinSelector :: Selector '[] (Id NSNumber)
holdTimeMinSelector = mkSelector "holdTimeMin"

-- | @Selector@ for @setHoldTimeMin:@
setHoldTimeMinSelector :: Selector '[Id NSNumber] ()
setHoldTimeMinSelector = mkSelector "setHoldTimeMin:"

-- | @Selector@ for @holdTimeMax@
holdTimeMaxSelector :: Selector '[] (Id NSNumber)
holdTimeMaxSelector = mkSelector "holdTimeMax"

-- | @Selector@ for @setHoldTimeMax:@
setHoldTimeMaxSelector :: Selector '[Id NSNumber] ()
setHoldTimeMaxSelector = mkSelector "setHoldTimeMax:"

-- | @Selector@ for @holdTimeDefault@
holdTimeDefaultSelector :: Selector '[] (Id NSNumber)
holdTimeDefaultSelector = mkSelector "holdTimeDefault"

-- | @Selector@ for @setHoldTimeDefault:@
setHoldTimeDefaultSelector :: Selector '[Id NSNumber] ()
setHoldTimeDefaultSelector = mkSelector "setHoldTimeDefault:"

